# List object aggregation types based on 'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

List object aggregation types based on
'https://www.entsoe.eu/Documents/EDI/Library/CodelistV94.zip'

## Usage

``` r
object_aggregation_types
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 16
rows and 3 columns.

## See also

Other types:
[`allocation_mode_types`](https://krose.github.io/entsoeapi/reference/allocation_mode_types.md),
[`analog_types`](https://krose.github.io/entsoeapi/reference/analog_types.md),
[`area_types`](https://krose.github.io/entsoeapi/reference/area_types.md),
[`asset_types`](https://krose.github.io/entsoeapi/reference/asset_types.md),
[`auction_types`](https://krose.github.io/entsoeapi/reference/auction_types.md),
[`business_types`](https://krose.github.io/entsoeapi/reference/business_types.md),
[`category_types`](https://krose.github.io/entsoeapi/reference/category_types.md),
[`classification_types`](https://krose.github.io/entsoeapi/reference/classification_types.md),
[`coding_scheme_types`](https://krose.github.io/entsoeapi/reference/coding_scheme_types.md),
[`connection_category_types`](https://krose.github.io/entsoeapi/reference/connection_category_types.md),
[`contract_types`](https://krose.github.io/entsoeapi/reference/contract_types.md),
[`coordinate_system_types`](https://krose.github.io/entsoeapi/reference/coordinate_system_types.md),
[`currency_types`](https://krose.github.io/entsoeapi/reference/currency_types.md),
[`curve_types`](https://krose.github.io/entsoeapi/reference/curve_types.md),
[`customer_types`](https://krose.github.io/entsoeapi/reference/customer_types.md),
[`direction_types`](https://krose.github.io/entsoeapi/reference/direction_types.md),
[`document_types`](https://krose.github.io/entsoeapi/reference/document_types.md),
[`eic_types`](https://krose.github.io/entsoeapi/reference/eic_types.md),
[`energy_product_types`](https://krose.github.io/entsoeapi/reference/energy_product_types.md),
[`flow_commodity_option_types`](https://krose.github.io/entsoeapi/reference/flow_commodity_option_types.md),
[`fuel_types`](https://krose.github.io/entsoeapi/reference/fuel_types.md),
[`grid_agreement_types`](https://krose.github.io/entsoeapi/reference/grid_agreement_types.md),
[`hvdc_mode_types`](https://krose.github.io/entsoeapi/reference/hvdc_mode_types.md),
[`indicator_types`](https://krose.github.io/entsoeapi/reference/indicator_types.md),
[`market_product_types`](https://krose.github.io/entsoeapi/reference/market_product_types.md),
[`message_types`](https://krose.github.io/entsoeapi/reference/message_types.md),
[`payment_terms_types`](https://krose.github.io/entsoeapi/reference/payment_terms_types.md),
[`price_category_types`](https://krose.github.io/entsoeapi/reference/price_category_types.md),
[`price_component_types`](https://krose.github.io/entsoeapi/reference/price_component_types.md),
[`price_direction_types`](https://krose.github.io/entsoeapi/reference/price_direction_types.md),
[`process_types`](https://krose.github.io/entsoeapi/reference/process_types.md),
[`quality_types`](https://krose.github.io/entsoeapi/reference/quality_types.md),
[`reason_code_types`](https://krose.github.io/entsoeapi/reference/reason_code_types.md),
[`rights_types`](https://krose.github.io/entsoeapi/reference/rights_types.md),
[`role_types`](https://krose.github.io/entsoeapi/reference/role_types.md),
[`settlement_method_types`](https://krose.github.io/entsoeapi/reference/settlement_method_types.md),
[`status_types`](https://krose.github.io/entsoeapi/reference/status_types.md),
[`sub_area_types`](https://krose.github.io/entsoeapi/reference/sub_area_types.md),
[`tariff_types`](https://krose.github.io/entsoeapi/reference/tariff_types.md),
[`timeframe_types`](https://krose.github.io/entsoeapi/reference/timeframe_types.md),
[`unit_multiplier`](https://krose.github.io/entsoeapi/reference/unit_multiplier.md),
[`unit_of_measure_types`](https://krose.github.io/entsoeapi/reference/unit_of_measure_types.md),
[`unit_symbol_types`](https://krose.github.io/entsoeapi/reference/unit_symbol_types.md)

## Examples

``` r
head(entsoeapi::object_aggregation_types)
#> # A tibble: 6 × 3
#>   code  title                    description                                                     
#>   <chr> <chr>                    <chr>                                                           
#> 1 A01   Area                     The object being described concerns an area.                    
#> 2 A02   Metering point           The object being described concerns a metering point.           
#> 3 A03   Party                    The object being described concerns a party.                    
#> 4 A04   Agreement Identification The object being described concerns an agreement identification.
#> 5 A05   Accounting point         The object being described concerns an accounting point.        
#> 6 A06   Resource Object          The object being described concerns a resource object.          
```
