fn main() -> u8 {
    let arr: i32[4];
    let arr_ptr: *i32 = arr as *i32;
    let double_cast: *void = arr as *i32 as *void;
    let void_ptr: *void = arr as *void;

    if arr_ptr == NULL {
        return 1;
    }

    return 0;
}
