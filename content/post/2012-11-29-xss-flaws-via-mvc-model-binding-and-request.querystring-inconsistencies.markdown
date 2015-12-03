---
categories:
- Security
- XSS
- ASP.NET
comments: true
date: 2012-11-29T00:00:00Z
title: XSS Flaws via MVC Model Binding and Request.QueryString Inconsistencies
---

Forgive the title of the post, it was hard coming up with something succinct that captured the purpose of the post. This was inspired by a recent experience with a client who had this exact problem with one of their production systems.

TL;DR
-----

Never use [Request.QueryString][] to access parameters in your views, even when you're sure your actions have validated them. You may open your application up to [XSS][] attacks. Always, _always_ use data that is passed to your views via the `Model` or the [ViewData][] dictionary. Under no circumstances should you trust data coming in from the web, that includes query string parameters.

<!--more-->

Setting the scene
-----------------

The application provided a certain function which allowed users to browse information tied to a number of entities. For the sake of this discussion let's say those entities were instances of a **Person**. The number of entities was extremely small, so it was decided that the interface would consist of a drop-down box consisting of all the entities. When that drop-down box was used a _change_ event would fire using JavaScript that would modify the current URL and render a new page. That page would show the same drop-down list, with the appropriate entry selected, along with the information specfic to that entity.

Let's see some code starting with the "details" of the person:

```
public class Person
{
    public string Name { get;set; }
    public int Age { get; set; }
}
```

Nothing too amazing there. Here's the parts of the page which renders the list of people:

```
Please choose a person <%= Html.DropDownList("people", Model) %>

<script type="text/javascript">
$(document).ready(function () {
  $("#people").change(function () {
    window.location = '<%= Url.Action("Detail", "Person") %>?index=' + $(this).val();
  });
});
</script>
```

Again, nothing too mind boggling here either. We can see how the `change` event fires and updates the `window.location` and moves the user to the `Details` page. The `Model` for this page is of type `IEnumerable<SelectListItem>`.

Next we'll take a look at the controller action which gets invoked when the drop-down is changed:

```
public ActionResult Detail(int index)
{
    return View("Detail", Tuple.Create(People[index], GetPeople()));
}
```

We're using the `index` parameter (note the _type_ of this parameter, it's important) to index into the list of people we have stashed somewhere to extract the user that is being viewed. This person, along with the full list of people in `SelectListItem` form, is being passed to the view.

Let's see what the view does with this information:

```
Please choose a person <%= Html.DropDownList("people", Model.Item2) %>

<script type="text/javascript">
$(document).ready(function () {
  $("#people").val(<%= Request.QueryString["index"] %>);
  $("#people").change(function () {
    window.location = '<%= Url.Action("Detail", "Person") %>?index=' + $(this).val();
  });
});
</script>

<p>
<%= Html.Encode(Model.Item1.Name) %> is a whopping <%= Model.Item1.Age %> years old!
</p>
```

This is starting to get a bit more interesting. The rendering of the drop-down is the same as before, except it's pulling data from the [System.Tuple][] that was passed in as the `Model`. The event handler that's invoked on `change` is the same, and there's a bit of non-descript content rendering a the bottom. The subtle but important difference is the code that sets the currently selected person in the drop-down.

I plugged this code into a standard MVC application and this is what it looks like:

{% img /uploads/2012/11/xss-index.png 'Choosing a person' %}

After selecting a person from the drop down it looks like this:

{% img /uploads/2012/11/xss-details.png 'Person details' %}

A bad assumption
----------------

As you already know, ASP.NET MVC has the ability to automatically convert query string parameters into type-safe arguments that are passed into the controller actions. In the above example the `index` query string parameter is converted to an `int` and passed into the `Details` action. If a user attempts to modify this value to something that isn't an integer then MVC will literally poop itself, right?

Let's see.

{% img /uploads/2012/11/xss-type-fail.png 'Person details' %}

OK, so MVC will require that this value be an integer, otherwise the action will fail to be invoked. If that's the case, then the view won't be rendered and our code which directly accesses `Request.QueryString` will not be invoked and hence there's no risk.

Right?

[Wrong][HPP].

The attack vector
-----------------

[HTTP Parameter Pollution][HPP] is a relatively "new" problem which revolves around the way that web servers/applications parse and handle multiple instances of the same GET/POST parameter. Phew!

In other words, if a malicious user decides to pass in _another_ instance of the `index` parameter in the query string, what happens? The way this is handled varies from technology to technology (see [slide 9][HPPPDF] for some examples), but here we're only interested in ASP.NET with IIS and MVC.

Let me just state this again: we're only interested in ASP.NET and IIS _**and MVC**_. The reason we need to emphasise this is because MVC rocks the boat a little bit. It's important to remember that [Request.QueryString][] is not specific to MVC, it's something that lives in ASP.NET land. _Controller actions_ on the other hand are not available across the board when using ASP.NET, they live in MVC land. When it comes to binding query string parameters to controller action method parameters, MVC doesn't behave how you would expect, and it's not the same as what happens when dealing with `Request.QueryString` directly.

So what happens in ASP.NET and IIS _without_ MVC if an attacker passes in two values for the same parameter? Consider the following url: `http://foo.com/bar?baz=0&baz=dooby`

While in C# land (remember, outside of MVC) `Request.QueryString["baz"]` will contain a string value `0,dooby`. In our current environment any duplicate instances of query string values are passed through as a single value joined together by commas. This knowledge is scarily uncommon. Many .NET developers that I know are not aware of this behaviour.

This is where MVC comes along and sprays poop over everything.

ASP.NET MVC controller action method parameter binding doesn't handle multiple instance of the same query string parameters in the same way. In fact, it behaves more like JSP on Jetty: **it only uses the first value and ignores the rest**.

Why is this bad? Well this means that code outside of controller actions may result in different behaviour to that which is inside. More importantly, accessing `Request.QueryString` to get access to a parameter doesn't mean that you'll get the same value as what was handled in the controller action.

Let's take a look at what might happen if we threw multiple instances via the query string at our dodgy MVC application:

{% img /uploads/2012/11/xss-hpp-invalid.png 'HPP example' %}

The above screenshot shows both the query string and the page source when we do this. That's right, MVC used the first value, set to `0`, without any issues and invoked the view engine. The view's assumption that the `index` parameter has been sanitised by the controller has now been proven unsafe. Now we can see that we do have access to the source. Let's see what happens if we went a step further and passed in a more sinister value:

{% img /uploads/2012/11/xss-hpp-valid.png 'HPP XSS exploit' %}

Pwned.

Conclusion
----------

_Never_ trust unsanitised input, that includes stuff that gets passed in through `Request.QueryString`. Do not assume that the contents are safe just because your controller action handled things without throwing an exception. If you're going to render data that has come in from the user or from the query string, delegate responsibility of validation to the controller action and only trust content which is passed to the view via [ViewData][] or the `Model`.

  [ViewData]: http://msdn.microsoft.com/en-us/library/system.web.mvc.viewpage.viewdata(v=vs.108).aspx
  [HPPPDF]: https://www.owasp.org/images/b/ba/AppsecEU09_CarettoniDiPaola_v0.8.pdf
  [Request.QueryString]: http://msdn.microsoft.com/en-us/library/ms524784(v=vs.90).aspx
  [XSS]: http://en.wikipedia.org/wiki/Cross-site_scripting
  [System.Tuple]: http://msdn.microsoft.com/en-us/library/system.tuple.aspx
  [HPP]: http://blog.iseclab.org/2010/12/08/http-parameter-pollution-so-how-many-flawed-applications-exist-out-there-we-go-online-with-a-new-service/

