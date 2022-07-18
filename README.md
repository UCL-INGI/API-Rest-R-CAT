# API-Rest-R-CAT

To run the api on port 8766:

```sh
r <- plumber::plumb("rest_api_cat.r")
r$run(host="0.0.0.0",port=8766)
```
