pub struct PPU {
    cycles: u64,
    pub draw_call: bool,
}

impl PPU {
    pub fn new() -> Self {
        PPU {
            cycles: 0,
            draw_call: false,
        }
    }

    pub fn draw(&mut self, cycles_passed: u64) {
        self.cycles += cycles_passed;
        if self.cycles > 70224 {
            self.cycles -= 70224;
            self.draw_call = true;
        } else {
            self.draw_call = false;
        }
    }
}
