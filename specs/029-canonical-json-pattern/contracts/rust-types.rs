//! Canonical JSON representation of Pattern<Subject>
//! Generated from gram-hs JSON Schema v0.1.0
//! @see <https://gram.data/schemas/pattern/v0.1.0/pattern.json>

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// A pattern with a subject value and nested elements
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Pattern {
    /// The subject value of this pattern
    pub value: Subject,
    /// Nested pattern elements
    pub elements: Vec<Pattern>,
}

/// A subject with identity, labels, and properties
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Subject {
    /// Identity of the subject - a symbol identifier (may be empty for anonymous subjects)
    pub identity: String,
    /// Type labels for the subject
    pub labels: Vec<String>,
    /// Property map with string keys and value types
    pub properties: HashMap<String, Value>,
}

/// A property value supporting multiple types
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Value {
    /// Integer value (JSON integer)
    Integer(i64),
    /// Decimal number value (JSON number)
    Decimal(f64),
    /// Boolean value
    Boolean(bool),
    /// String value
    String(String),
    /// Symbol value
    Symbol(ValueSymbol),
    /// Tagged string value with metadata
    TaggedString(ValueTaggedString),
    /// Numeric range value
    Range(ValueRange),
    /// Measurement value with unit
    Measurement(ValueMeasurement),
    /// Array of values
    Array(Vec<Value>),
    /// Map of string keys to values
    Map(HashMap<String, Value>),
}

/// A symbol value
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ValueSymbol {
    /// Type discriminator (always "symbol")
    #[serde(rename = "type")]
    pub type_: String,
    /// The symbol identifier
    pub value: String,
}

impl ValueSymbol {
    /// Create a new ValueSymbol
    pub fn new(value: impl Into<String>) -> Self {
        Self {
            type_: "symbol".to_string(),
            value: value.into(),
        }
    }
}

/// A tagged string value with metadata
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ValueTaggedString {
    /// Type discriminator (always "tagged")
    #[serde(rename = "type")]
    pub type_: String,
    /// Tag identifying the string format (e.g., 'json', 'html')
    pub tag: String,
    /// The string content
    pub content: String,
}

impl ValueTaggedString {
    /// Create a new ValueTaggedString
    pub fn new(tag: impl Into<String>, content: impl Into<String>) -> Self {
        Self {
            type_: "tagged".to_string(),
            tag: tag.into(),
            content: content.into(),
        }
    }
}

/// A numeric range value
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ValueRange {
    /// Type discriminator (always "range")
    #[serde(rename = "type")]
    pub type_: String,
    /// Lower bound of the range (inclusive)
    pub lower: f64,
    /// Upper bound of the range (inclusive)
    pub upper: f64,
}

impl ValueRange {
    /// Create a new ValueRange
    ///
    /// # Panics
    /// Panics if lower > upper
    pub fn new(lower: f64, upper: f64) -> Self {
        assert!(lower <= upper, "lower bound must be <= upper bound");
        Self {
            type_: "range".to_string(),
            lower,
            upper,
        }
    }
}

/// A measurement value with unit
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ValueMeasurement {
    /// Type discriminator (always "measurement")
    #[serde(rename = "type")]
    pub type_: String,
    /// Unit of measurement (e.g., 'kg', '°C', 'm/s')
    pub unit: String,
    /// Numeric measurement value
    pub value: f64,
}

impl ValueMeasurement {
    /// Create a new ValueMeasurement
    pub fn new(unit: impl Into<String>, value: f64) -> Self {
        Self {
            type_: "measurement".to_string(),
            unit: unit.into(),
            value,
        }
    }
}

// Convenience methods for creating Value variants
impl Value {
    /// Create a symbol value
    pub fn symbol(value: impl Into<String>) -> Self {
        Value::Symbol(ValueSymbol::new(value))
    }

    /// Create a tagged string value
    pub fn tagged_string(tag: impl Into<String>, content: impl Into<String>) -> Self {
        Value::TaggedString(ValueTaggedString::new(tag, content))
    }

    /// Create a range value
    pub fn range(lower: f64, upper: f64) -> Self {
        Value::Range(ValueRange::new(lower, upper))
    }

    /// Create a measurement value
    pub fn measurement(unit: impl Into<String>, value: f64) -> Self {
        Value::Measurement(ValueMeasurement::new(unit, value))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_serialize_simple_pattern() {
        let pattern = Pattern {
            value: Subject {
                identity: "test".to_string(),
                labels: vec!["Node".to_string()],
                properties: HashMap::new(),
            },
            elements: vec![],
        };

        let json = serde_json::to_string(&pattern).unwrap();
        assert!(json.contains("\"identity\":\"test\""));
        assert!(json.contains("\"labels\":[\"Node\"]"));
    }

    #[test]
    fn test_deserialize_simple_pattern() {
        let json = r#"{
            "value": {
                "identity": "test",
                "labels": ["Node"],
                "properties": {}
            },
            "elements": []
        }"#;

        let pattern: Pattern = serde_json::from_str(json).unwrap();
        assert_eq!(pattern.value.identity, "test");
        assert_eq!(pattern.value.labels, vec!["Node"]);
        assert!(pattern.value.properties.is_empty());
        assert!(pattern.elements.is_empty());
    }

    #[test]
    fn test_value_symbol() {
        let val = Value::symbol("identifier");
        let json = serde_json::to_string(&val).unwrap();
        assert!(json.contains("\"type\":\"symbol\""));
        assert!(json.contains("\"value\":\"identifier\""));
    }

    #[test]
    fn test_value_range() {
        let val = Value::range(1.0, 10.0);
        let json = serde_json::to_string(&val).unwrap();
        assert!(json.contains("\"type\":\"range\""));
        assert!(json.contains("\"lower\":1"));
        assert!(json.contains("\"upper\":10"));
    }

    #[test]
    #[should_panic(expected = "lower bound must be <= upper bound")]
    fn test_invalid_range() {
        ValueRange::new(10.0, 1.0);
    }
}
