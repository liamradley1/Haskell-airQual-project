# functional-assignment

- IMPORTANT NOTE: Typing "SET CLIENT_ENCODING TO 'utf-8'" in your PSQL shell allows for return of characters not in   WIN1252 encoding. This is especially important for this project, as character types may be returned from languages that do not use the Latin alphabet. Failure to do this may cause queries throw errors, and will fail to return any values.
- A maximum of 2,000,000 entries can be requested, or 2,000 requests to the server, whichever is reached first. Be sure not to exceed this, otherwise requests will be denied.
