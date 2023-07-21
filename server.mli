val json_from_uri : string -> Yojson.Basic.t
(** [json_from_uri uri] gets a json from the endpoint at [uri]. requires: [uri]
    is a valid uri to an endpoint that returns a json on a get request *)
