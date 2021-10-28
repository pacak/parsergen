#[cfg(test)]
mod tests;
pub fn experiment<'a>(
    raw: &'a [u8; 3usize + 3usize],
    res: &'a mut (u32, u32),
) -> ::parsergen::Result<'a, ()> {
    const O_0: usize = 0;
    const O_1: usize = 0 + 3usize;
    const O_2: usize = O_1 + 3usize;
    let slice = ::arrayref::array_ref!(raw, O_0, O_1 - O_0);
    (|| {
        let mut val = 0;
        let a = ::arrayref::array_ref!(slice, 0usize, 3usize);
        let v = ::parsergen::primitives::numbers::fold_digits::<u32>(a)?;
        val += v as u32;
        res.0 = val;
        Some(())
    })()
    .ok_or_else(|| ::parsergen::Error {
        _msg: "Not a valid number",
        _payload: raw,
    })?;
    let slice = ::arrayref::array_ref!(raw, O_1, O_2 - O_1);
    (|| {
        let mut val = 0;
        let a = ::arrayref::array_ref!(slice, 0usize, 3usize);
        let v = ::parsergen::primitives::numbers::fold_digits::<u32>(a)?;
        res.1 = v as u32;
        Some(())
    })()
    .ok_or_else(|| ::parsergen::Error {
        _msg: "Not a valid number",
        _payload: raw,
    })?;
    Ok(())
}
