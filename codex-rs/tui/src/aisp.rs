use codex_common::aisp::AispConversion;
use codex_common::aisp::convert_prompt_to_aisp;

pub fn convert_prompt_for_agent(prompt: &str) -> AispConversion {
    if cfg!(test) {
        return AispConversion {
            converted: prompt.to_string(),
            changed: false,
        };
    }

    convert_prompt_to_aisp(prompt)
}
