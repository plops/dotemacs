#!/bin/bash

# Default values
KEEP_FILES=false
WHISPER_MODEL_PATH="/home/kiel/src/whisper.cpp/models/ggml-large-v3-turbo-q8_0.bin"
WHISPER_CLI_PATH="~/src/whisper.cpp/build/bin/whisper-cli" #<-- UPDATE this path to your whisper-cli executable

# NEW: default language (auto by default)
LANGUAGE="auto"

# Expand tilde if present in WHISPER_CLI_PATH so the path actually resolves
WHISPER_CLI_PATH="${WHISPER_CLI_PATH/#\~/$HOME}"

# Function to display usage information
usage() {
    echo "Usage: $0 [options] <video_file>"
    echo
    echo "Options:"
    echo "  -k, --keep        Keep intermediate audio file."
    echo "  -L, --lang LANG   Language to use (default: auto)."
    echo "  -h, --help        Display this help message."
    exit 1
}

# Parse command-line arguments
while [[ "$#" -gt 0 ]]; do
    case $1 in
        -k|--keep) KEEP_FILES=true; shift ;;
        -L|--lang)
            if [ -n "$2" ] && [[ "$2" != -* ]]; then
                LANGUAGE="$2"
                shift 2
            else
                echo "Error: --lang requires an argument."
                usage
            fi
            ;;
        -h|--help) usage ;;
        *)
            if [ -z "$VIDEO_FILE" ]; then
                VIDEO_FILE="$1"
            else
                echo "Error: Unexpected argument '$1'"
                usage
            fi
            shift ;;
    esac
done

# Check if a video file was provided
if [ -z "$VIDEO_FILE" ]; then
    echo "Error: No video file specified."
    usage
fi

# Check if the video file exists
if [ ! -f "$VIDEO_FILE" ]; then
    echo "Error: File not found: $VIDEO_FILE"
    exit 1
fi

# Set output file names
BASENAME=$(basename "$VIDEO_FILE" | sed 's/\.[^.]*$//')
OUTPUT_TXT_FILE="${BASENAME}.txt"
TEMP_AUDIO_FILE="${BASENAME}_temp_audio.wav"

# Basic checks for whisper-cli/model to catch common errors early
if [ ! -x "$WHISPER_CLI_PATH" ]; then
    echo "Warning: whisper-cli not found or not executable at: $WHISPER_CLI_PATH"
    echo "Make sure the path is correct and executable. Continuing anyway..."
fi

if [ ! -f "$WHISPER_MODEL_PATH" ]; then
    echo "Warning: Whisper model not found at: $WHISPER_MODEL_PATH"
    echo "Transcription will likely fail if the model path is incorrect."
fi

# --- 1. Extract Audio from Video ---
echo "Extracting audio from '$VIDEO_FILE'..."
ffmpeg -i "$VIDEO_FILE" -ar 16000 -ac 1 -c:a pcm_s16le "$TEMP_AUDIO_FILE" -hide_banner -loglevel error

if [ $? -ne 0 ]; then
    echo "Error: ffmpeg failed to extract audio."
    exit 1
fi

# --- 2. Convert Audio to Text with Timestamps ---
echo "Transcribing audio to text (language: $LANGUAGE)..."
# The -osrt flag outputs srt; pass the language option from $LANGUAGE.
# Invoke whisper-cli directly (no eval) and quote args to avoid word-splitting issues.
"$WHISPER_CLI_PATH" -m "$WHISPER_MODEL_PATH" -l "$LANGUAGE" -osrt -f "$TEMP_AUDIO_FILE" -of "$BASENAME"

if [ $? -ne 0 ]; then
    echo "Error: whisper.cpp failed to transcribe the audio."
    # Clean up the temporary audio file even if transcription fails
    if [ "$KEEP_FILES" = false ]; then
        rm "$TEMP_AUDIO_FILE"
    fi
    exit 1
fi

echo "Transcription complete. Output saved to '$OUTPUT_TXT_FILE'"

# --- 3. Clean up intermediate files ---
if [ "$KEEP_FILES" = false ]; then
    echo "Removing intermediate audio file..."
    rm "$TEMP_AUDIO_FILE"
fi

echo "Done."
