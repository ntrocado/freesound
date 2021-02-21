# Common Lisp client for the Freesound API

[Freesound](https://freesound.org/) is a large scale collaborative database of audio recordings released under Creative Commons licenses. Is offers an API for interacting with the database: searching for sounds, retrieving metadata, accessing audio content analysis, download and upload sounds, comment, rate, and bookmark, etc.

This is a Common Lisp library for accessing the API. It's created and maintained by [Nuno Trocado](htts://nunotrocado.com), and released under the [MIT License](https://github.com/ntrocado/freesound/blob/master/LICENSE).

## Authentication

The Freesound API offers two authentication strategies: token-based and OAuth2. The first one is simpler, but you need OAuth2 to access resources that imply a logged in user, including downloading sound (but not lossy preview versions).

### Token authentication

To use the token authentication strategy, go to Freesound and [request](https://freesound.org/apiv2/apply) new API credentials. Then you'll be shown a table with the requested credentials. Use the alphanumeric string in the "Client secret/Api key" column:

``` lisp
(setf *token* <string>)
```

If you save it on a ".token" file on the same directory as the `freesound` system, the code will be read everytime the system is loaded.

### OAuth2 authentication

In addition to the previous steps, find the "client id" at the same credentials table in the Freesound website. Then do:

``` lisp
(setf *client-id* <string>)
(oauth2-authorize)
```

The default browser will be opened and you'll be asked to log in and authorize the access to Freesound. The link will be also printed (tipically to the REPL). Afterwards, you'll be redirected to another Freesound page showing you a new alphanumeric code. Copy this code and do:

``` lisp
(oauth2-get-tokens <code>)
```

Done. This function will also return two values: the access token used for all OAuth2 resources, and a refresh token. If you save the refresh token you can spare yourself (or the user of your app) the full authorization process, by using `(oauth2-get-tokens <refresh token> :refresh t)` each time the authorization times out, which it does after 24 hours).

## Examples

Perform a text search, using a minus '-' character to exclude terms from the search:
``` lisp
(text-search "conga percussive -loop")
```

This is equivalent:
``` lisp
(text-search '("conga" "percussive" "-loop"))
```

`print-search-result` is a convenience function to pretty print the results. Here's another example:

``` lisp
(print-search-result (text-search "piano"
		     		  :filter '(:duration (:range-to 2))
				  :sort "rating_desc" :page-size 3
				  :fields '("id" "name" "analysis")
				  :descriptors '("lowlevel.spectral_centroid.mean"
						 "lowlevel.pitch.mean")))
```
Which prints:
```
The search returned 3 results from a total of 943:

id:            442981
name:          Sol-G.m4a
analysis:      lowlevel:      spectral_cent: mean:          499.9333
                              pitch:         mean:          386.42862

id:            39210
name:          Piano ff 062.wav
analysis:      lowlevel:      spectral_cent: mean:          1047.3967
                              pitch:         mean:          933.2059

id:            337302
name:          MSfxP6 - 183 - One Shot Sound 3
analysis:      lowlevel:      spectral_cent: mean:          435.96042
                              pitch:         mean:          460.73337
#<HASH-TABLE :TEST EQUAL :COUNT 4 {1004F4DF73}>
```