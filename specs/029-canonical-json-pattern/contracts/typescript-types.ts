/**
 * Canonical JSON representation of Pattern<Subject>
 * Generated from gram-hs JSON Schema v0.1.0
 * @see https://gram.data/schemas/pattern/v0.1.0/pattern.json
 */

/**
 * A pattern with a subject value and nested elements
 */
export interface Pattern {
  /** The subject value of this pattern */
  value: Subject;
  /** Nested pattern elements */
  elements: Pattern[];
}

/**
 * A subject with identity, labels, and properties
 */
export interface Subject {
  /** Identity of the subject - a symbol identifier (may be empty for anonymous subjects) */
  identity: string;
  /** Type labels for the subject */
  labels: string[];
  /** Property map with string keys and value types */
  properties: { [key: string]: Value };
}

/**
 * A property value supporting multiple types
 */
export type Value =
  | number
  | boolean
  | string
  | ValueSymbol
  | ValueTaggedString
  | ValueRange
  | ValueMeasurement
  | Value[]
  | { [key: string]: Value };

/**
 * A symbol value
 */
export interface ValueSymbol {
  type: "symbol";
  /** The symbol identifier */
  value: string;
}

/**
 * A tagged string value with metadata
 */
export interface ValueTaggedString {
  type: "tagged";
  /** Tag identifying the string format (e.g., 'json', 'html') */
  tag: string;
  /** The string content */
  content: string;
}

/**
 * A numeric range value
 */
export interface ValueRange {
  type: "range";
  /** Lower bound of the range (inclusive) */
  lower: number;
  /** Upper bound of the range (inclusive) */
  upper: number;
}

/**
 * A measurement value with unit
 */
export interface ValueMeasurement {
  type: "measurement";
  /** Unit of measurement (e.g., 'kg', '°C', 'm/s') */
  unit: string;
  /** Numeric measurement value */
  value: number;
}

/**
 * Type guard to check if a value is a ValueSymbol
 */
export function isValueSymbol(value: Value): value is ValueSymbol {
  return (
    typeof value === "object" &&
    value !== null &&
    "type" in value &&
    value.type === "symbol"
  );
}

/**
 * Type guard to check if a value is a ValueTaggedString
 */
export function isValueTaggedString(value: Value): value is ValueTaggedString {
  return (
    typeof value === "object" &&
    value !== null &&
    "type" in value &&
    value.type === "tagged"
  );
}

/**
 * Type guard to check if a value is a ValueRange
 */
export function isValueRange(value: Value): value is ValueRange {
  return (
    typeof value === "object" &&
    value !== null &&
    "type" in value &&
    value.type === "range"
  );
}

/**
 * Type guard to check if a value is a ValueMeasurement
 */
export function isValueMeasurement(value: Value): value is ValueMeasurement {
  return (
    typeof value === "object" &&
    value !== null &&
    "type" in value &&
    value.type === "measurement"
  );
}

/**
 * Type guard to check if a value is an array
 */
export function isValueArray(value: Value): value is Value[] {
  return Array.isArray(value);
}

/**
 * Type guard to check if a value is a map/object
 */
export function isValueMap(value: Value): value is { [key: string]: Value } {
  return (
    typeof value === "object" &&
    value !== null &&
    !Array.isArray(value) &&
    !("type" in value)
  );
}

/**
 * Type guard to check if a value is a primitive (number, boolean, or string)
 */
export function isValuePrimitive(
  value: Value
): value is number | boolean | string {
  return (
    typeof value === "number" ||
    typeof value === "boolean" ||
    typeof value === "string"
  );
}
