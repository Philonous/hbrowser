#include "setproxy.h"

void setProxy(const char *szUri)
{
  SoupURI *soupUri = szUri ? soup_uri_new(szUri) : 0;
  g_object_set(webkit_get_default_session(), SOUP_SESSION_PROXY_URI, soupUri, NULL);
  if (soupUri)
    soup_uri_free(soupUri);
}
