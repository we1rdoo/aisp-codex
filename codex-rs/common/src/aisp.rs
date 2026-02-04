#[derive(Debug, Clone)]
pub struct AispConversion {
    pub converted: String,
    pub changed: bool,
}

/// Convert a plain-text prompt into an AISP specification using `rosetta-aisp`.
///
/// The converter returns the original prompt if it cannot produce output or
/// if the conversion would be empty.
pub fn convert_prompt_to_aisp(prompt: &str) -> AispConversion {
    let trimmed_input = prompt.trim();
    if trimmed_input.is_empty() {
        return AispConversion {
            converted: prompt.to_string(),
            changed: false,
        };
    }

    let result = rosetta_aisp::AispConverter::convert(trimmed_input, None);
    let converted = result.output.trim();
    let converted = if converted.is_empty() {
        prompt.to_string()
    } else {
        converted.to_string()
    };

    let changed = converted != prompt;

    AispConversion { converted, changed }
}
