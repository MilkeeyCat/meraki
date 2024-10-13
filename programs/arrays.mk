fn main() -> u8 {
    let arr: i32[4];
    let arr_ptr: *i32 = arr as *i32;

    if arr_ptr == NULL {
        return 1;
    }

    return 0;
}
