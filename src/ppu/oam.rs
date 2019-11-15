bitfield! {
    #[derive(Clone, Copy)]
    pub struct OAMEntryAttrs(u8);
    impl Debug;

    pub palette_idx, set_palette_idx: 1, 0;

    pub priority, set_priority: 5;

    pub horiz_flip, set_horiz_flip: 6;

    pub vert_flip, set_vert_flip: 7;
}

#[derive(Clone, Copy, Debug)]
pub struct OAMEntry {
    pub y: u8,
    pub index: u8,
    pub attrs: OAMEntryAttrs,
    pub x: u8,
}

impl Default for OAMEntry {
    fn default() -> OAMEntry {
        OAMEntry {
            y: 0,
            index: 0,
            attrs: OAMEntryAttrs(0),
            x: 0,
        }
    }
}


