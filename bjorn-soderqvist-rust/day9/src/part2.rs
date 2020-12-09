pub fn calculate(code: Vec<usize>) -> usize {
    hack_xmas2(&code, 22477624)
}

fn get_target_pos(n_sequence: &Vec<usize>, target: usize) -> usize {
    let n_sequence_len = n_sequence.len();
    for i in 0..n_sequence_len {
        if n_sequence[i] == target { return i }
    }
    panic!("Target {} not found in this sequence");
}

fn hack_xmas2(n_sequence: &Vec<usize>, target: usize) -> usize {
    let iter_max = get_target_pos(n_sequence, target);
    for i in 0..iter_max {
        let mut sum = 0;
        for j in i..iter_max {
            sum += n_sequence[j];
            if sum == target {
                // Determine lowest and highest number, add them together.
                let mut final_sequence: Vec<usize> = n_sequence.get(i..j).unwrap().to_vec();
                final_sequence.sort();
                return final_sequence[0] + final_sequence[final_sequence.len() - 1];
            }
            if sum > target {
                break;
            }
        }
    }
    panic!("This stream is unhackable!!1");
}
