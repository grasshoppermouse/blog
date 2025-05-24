return {
  ['orcid'] = function(args, kwargs, meta)
    local var = pandoc.utils.stringify(args[1])
    local url = string.format("https://orcid.org/%s", var)
    return pandoc.Link(var, url)
  end
}
