{- HLINT ignore "Use camelCase" -}
module Groq.Types.Models
  ( ModelId(..)
  , fromId
  ) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Text qualified as Text
import Data.Default

data ModelId
  = Model_meta_llama_llama_4_maverick_17b_128e_instruct
  | Model_openai_gpt_oss_120b
  | Model_groq_compound_mini
  | Model_groq_compound
  | Model_llama_3_3_70b_versatile
  | Model_moonshotai_kimi_k2_instruct
  | Model_meta_llama_llama_prompt_guard_2_86m
  | Model_openai_gpt_oss_20b
  | Model_playai_tts_arabic
  | Model_meta_llama_llama_4_scout_17b_16e_instruct
  | Model_openai_gpt_oss_safeguard_20b
  | Model_whisper_large_v3_turbo
  | Model_allam_2_7b
  | Model_meta_llama_llama_prompt_guard_2_22m
  | Model_llama_3_1_8b_instant
  | Model_meta_llama_llama_guard_4_12b
  | Model_playai_tts
  | Model_moonshotai_kimi_k2_instruct_0905
  | Model_qwen_qwen3_32b
  | Model_whisper_large_v3
  deriving (Eq, Ord, Enum, Bounded)

instance Default ModelId where
  def = Model_groq_compound_mini

instance Aeson.ToJSON ModelId where
  toJSON = Aeson.String . Text.pack . show

instance Aeson.FromJSON ModelId where
    parseJSON (Aeson.String m) = 
      case fromId (Text.unpack m) of
        Just model -> pure model
        Nothing -> Aeson.parseFail "Model failed to parse"
    parseJSON invalid =
        Aeson.prependFailure "Model failed"
            (Aeson.typeMismatch "String" invalid)

instance Show ModelId where
  show Model_meta_llama_llama_4_maverick_17b_128e_instruct =
    "meta-llama/llama-4-maverick-17b-128e-instruct"
  show Model_openai_gpt_oss_120b =
    "openai/gpt-oss-120b"
  show Model_groq_compound_mini =
    "groq/compound-mini"
  show Model_groq_compound =
    "groq/compound"
  show Model_llama_3_3_70b_versatile =
    "llama-3.3-70b-versatile"
  show Model_moonshotai_kimi_k2_instruct =
    "moonshotai/kimi-k2-instruct"
  show Model_meta_llama_llama_prompt_guard_2_86m =
    "meta-llama/llama-prompt-guard-2-86m"
  show Model_openai_gpt_oss_20b =
    "openai/gpt-oss-20b"
  show Model_playai_tts_arabic =
    "playai-tts-arabic"
  show Model_meta_llama_llama_4_scout_17b_16e_instruct =
    "meta-llama/llama-4-scout-17b-16e-instruct"
  show Model_openai_gpt_oss_safeguard_20b =
    "openai/gpt-oss-safeguard-20b"
  show Model_whisper_large_v3_turbo =
    "whisper-large-v3-turbo"
  show Model_allam_2_7b =
    "allam-2-7b"
  show Model_meta_llama_llama_prompt_guard_2_22m =
    "meta-llama/llama-prompt-guard-2-22m"
  show Model_llama_3_1_8b_instant =
    "llama-3.1-8b-instant"
  show Model_meta_llama_llama_guard_4_12b =
    "meta-llama/llama-guard-4-12b"
  show Model_playai_tts =
    "playai-tts"
  show Model_moonshotai_kimi_k2_instruct_0905 =
    "moonshotai/kimi-k2-instruct-0905"
  show Model_qwen_qwen3_32b =
    "qwen/qwen3-32b"
  show Model_whisper_large_v3 =
    "whisper-large-v3"

fromId :: String -> Maybe ModelId
fromId = \case
  "meta-llama/llama-4-maverick-17b-128e-instruct" ->
    Just Model_meta_llama_llama_4_maverick_17b_128e_instruct
  "openai/gpt-oss-120b" ->
    Just Model_openai_gpt_oss_120b
  "groq/compound-mini" ->
    Just Model_groq_compound_mini
  "groq/compound" ->
    Just Model_groq_compound
  "llama-3.3-70b-versatile" ->
    Just Model_llama_3_3_70b_versatile
  "moonshotai/kimi-k2-instruct" ->
    Just Model_moonshotai_kimi_k2_instruct
  "meta-llama/llama-prompt-guard-2-86m" ->
    Just Model_meta_llama_llama_prompt_guard_2_86m
  "openai/gpt-oss-20b" ->
    Just Model_openai_gpt_oss_20b
  "playai-tts-arabic" ->
    Just Model_playai_tts_arabic
  "meta-llama/llama-4-scout-17b-16e-instruct" ->
    Just Model_meta_llama_llama_4_scout_17b_16e_instruct
  "openai/gpt-oss-safeguard-20b" ->
    Just Model_openai_gpt_oss_safeguard_20b
  "whisper-large-v3-turbo" ->
    Just Model_whisper_large_v3_turbo
  "allam-2-7b" ->
    Just Model_allam_2_7b
  "meta-llama/llama-prompt-guard-2-22m" ->
    Just Model_meta_llama_llama_prompt_guard_2_22m
  "llama-3.1-8b-instant" ->
    Just Model_llama_3_1_8b_instant
  "meta-llama/llama-guard-4-12b" ->
    Just Model_meta_llama_llama_guard_4_12b
  "playai-tts" ->
    Just Model_playai_tts
  "moonshotai/kimi-k2-instruct-0905" ->
    Just Model_moonshotai_kimi_k2_instruct_0905
  "qwen/qwen3-32b" ->
    Just Model_qwen_qwen3_32b
  "whisper-large-v3" ->
    Just Model_whisper_large_v3
  _ ->
    Nothing
