include_directories(${HELLO_SOURCE_DIR}/lib)
link_directories(${HELLO_BINARY_DIR}/hellolib)

add_executable(hello main.c)

target_link_libraries(hello hellolib)
