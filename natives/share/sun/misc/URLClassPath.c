#include <linkage.h>
#include <natives-dsl.h>

DECLARE_NATIVE("sun/misc", URLClassPath, getLookupCacheURLs,
               "(Ljava/lang/ClassLoader;)[Ljava/net/URL;") {
  // Return an empty array
  classdesc *URL = bootstrap_lookup_class(thread, STR("java/net/URL"));
  link_class(thread, URL);
  obj_header *array = CreateObjectArray1D(thread, URL, 0);
  return (stack_value){.obj = array};
}