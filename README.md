flextml
=======

Utility to transform cute HTML into a valid HTML 5.
Currently supports addition of "data-" prefixes to custom attributes,
and transformation of custom tags into divs with corresponding boolean attribute.

Could be useful for angular's custom tag and attribute directives.

Transformation example:

```html
<field label="Tratata: " class="controls">
  <autocomplete source="myYobaFn" value="someField"></autocomplete>
</field>
```

→

```html
<div data-field="" data-label="Tratata: " class="controls">
  <div data-autocomplete="" data-source="myYobaFn" data-value="someField"></div>
</div>
```
