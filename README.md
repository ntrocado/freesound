# Common Lisp client for the Freesound API

[Freesound](https://freesound.org/) is a large-scale collaborative database of audio recordings released under Creative Commons licenses. It offers an API for interacting with the database: searching for sounds, retrieving meta-data, accessing audio content analysis, download and upload sounds, comment, rate, and bookmark, etc.

This is a Common Lisp library for the API, created and maintained by [Nuno Trocado](https://nunotrocado.com), and released under the [MIT License](https://github.com/ntrocado/freesound/blob/master/LICENSE).

## Authentication

The Freesound API offers two authentication strategies: token-based and OAuth2. The first one is simpler, but you need OAuth2 to access resources that imply a logged in user, including downloading sounds (but not lossy preview versions).

### Token authentication

To use the token authentication strategy, [request new API credentials](https://freesound.org/apiv2/apply) at Freesound. Then you'll be shown a table with the requested credentials. Set `*token*` to the alphanumeric string found in the "Client secret/Api key" column:

``` lisp
(setf *token* <string>)
```

If you save it on a ".token" file on the same directory as the `freesound` system, the code will be read automatically every time the system is loaded.

### OAuth2 authentication

In addition to the previous steps, find the "client id" at the same credentials table in the Freesound website. Then do:

``` lisp
(setf *client-id* <string>)
(oauth2-authorize)
```

The default browser will be opened and you'll be asked to log in and authorize the access to Freesound. The specific link will be also printed (typically to the REPL). Afterwards, you'll be redirected to another Freesound page showing you a new alphanumeric code. Copy this code and do:

``` lisp
(oauth2-get-tokens <code>)
```

This function stores the oauth2 token in `*oauth2-access-token*`, which will be used in further API calls. It will also return two values: the access token itself and a refresh token. If you save the refresh token you can spare yourself (or the user of your app) the full authorization process, by using `(oauth2-get-tokens <refresh token> :refresh t)` each time the authorization times out, which it does after 24 hours.

## Documentation

Full documentation is [here](http://nunotrocado.com/freesound/).

Refer to the [Freesound API documentation](https://freesound.org/docs/api/index.html) for the complete description of the available resources and their request parameters and accepted values, as well as the format of the responses.

The responses obtained from Freesound are translated to Common Lisp data structures. In particular, objects like search results and instance information are parsed as hash tables.

For the request parameters, a string conforming to the API syntax is always accepted. When multiple terms are to be passed to a single parameter, lists are also accepted, and converted to the appropriate strings under the hood. Numbers are automatically parsed too.

In some cases, there are further facilities to construct parameters in a lispy way. Check the [examples](#examples) below and the [reference](http://nunotrocado.com/freesound/).

There are also convenience functions to make it easier to perform quick queries from the REPL: `print-search-result` and `print-info`.

## Overview and examples

### Searching

Perform a text search, using the minus '-' character to exclude terms from the search:

``` lisp
(text-search "conga percussive -loop")
```

This is equivalent:

``` lisp
(text-search '("conga" "percussive" "-loop"))
```

In general, any request parameters can be passed either as a single string conforming to the API syntax, or as a list of terms.

Filter properties can be further specified with the operators `:and`, `:or`, `:range`, `:range-to`, and `:range-from`. For example:

``` lisp
(text-search "rain" 
             :filter '((:tag (:and "soundscape" "forest")) 
                       (:created (:range-from "2010-12-01T23:59:59.999Z")) 
                       (:duration (:range 10 120))))
```

`print-search-result` is a convenience function to pretty print the results.

Here's another example:

``` lisp
(print-search-result (text-search "piano"
                                  :filter '(:duration (:range-to 2))
                                  :sort "rating_desc"
                                  :page-size 3
                                  :fields '("id" "name" "analysis")
                                  :descriptors '("lowlevel.spectral_centroid.mean"
                                                 "lowlevel.pitch.mean")))
```

Which prints:

```
Showing 3 results from a total of 943:

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
```

As you can see, the response included the fields and descriptors chosen, and was limited to 3 results, as this was the `page-size` that we specified.

The descriptors (audio features) can also be used as the search query, with `content-search`:

``` lisp
(content-search '("lowlevel.pitch.mean" 220)
                :page-size 10
		:fields '("id" "tags"))
```

The results are sorted according to their distance from the descriptor value.

The target for `content-search` can also be another sound. In this case, the target sound's descriptors will be used as the search query, but it's possible to further limit the search using `:descriptors-filter`, which accepts the same constructs as `:filter` in `text-search`.

``` lisp
(content-search 39210
		:descriptors-filter '(("tonal.key_key" "\"A\"")
                                      ("tonal.key_strength" (:range-from 0.8))))
```

Note that for parameters that must be enclosed in double-quotes, we have to escape the double-quotes, like `"A"` above.

You can achieve the same kind of search with `similar`.

Finally, it's also possible to search in meta-data and audio features at the same time, with `combined-search`.

### Sounds

Use `info` to get information on a sound from the Freesound database. For example:

``` lisp
(info 1234 :fields "filesize")
```

If you're calling `info` after performing a search, it's recommended that you include the `fields` parameter with the search request, removing the need to make an extra query for each individual result.

`print-info` is useful to quickly get the values at the REPL:

``` lisp
(print-info (info 213524
		  :fields '("name" "analysis")
		  :descriptors '("rhythm.bpm"
				 "lowlevel.mfcc.min"
				 "lowlevel.spectral_centroid.mean"
				 "tonal.hpcp_entropy.var")))
```

Prints:

```
name:          120 bpm distorded drum loop
analysis:      lowlevel:      spectral_cent: mean:          1313.7423
                              mfcc:          min:           (-1011.6488
                                                             -21.778429
                                                             -79.655426
                                                             -22.136934
                                                             -39.293144
                                                             -32.40432
                                                             -29.894882
                                                             -21.09641
                                                             -27.268171
                                                             -25.490017
                                                             -66.50812
                                                             -33.063526
                                                             -21.2058)
               tonal:         hpcp_entropy:  var:           0.45190468
               rhythm:        bpm:           119.80438
```

Use `preview` to download lossy versions of sounds:

``` lisp
(preview 678 "~/678.mp3")
```

And `download` for other formats, e.g.:

``` lisp
(download 345 (merge-pathnames (gethash "name" (info 345))
                               (user-homedir-pathname)))
```

You can also upload your sounds with `upload`, either describing them at the same time or using `describe-sound` later. Check the list of sounds pending approval with `pending-uploads`.

Note that OAuth2 authentication is required for downloading (but not with `preview`) and uploading.

Other functions that operate on sounds are `comment`, `edit-sound-description`, `bookmark`, and `rate`.

### Users, packs, and more

There's a range of functions that return information on a Freesound user. Check the documentation. They start with `user-`.

Similarly, you can get information and operate on packs with the functions that start with `pack-`.

`me` returns information on the logged-in user, and `descriptors` returns the available audio descriptor names, organized by category.

## Contributing

Feedback, questions, bug reports, pull requests, etc. are very welcomed!
