include(FetchContent)
FetchContent_Declare(
        doctest
        GIT_REPOSITORY https://github.com/doctest/doctest
        GIT_TAG master # or any SHA, Branch or tag like v2.4.9
)
FetchContent_MakeAvailable(doctest)