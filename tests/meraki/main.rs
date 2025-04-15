mod run;

use run::run;
use serde::Deserialize;
use std::io::BufReader;

#[derive(Debug, Deserialize)]
struct Expected {
    exit_code: i32,
    // Here should also be stdout but rn I can't print anything out xd
}

#[test]
fn programs() -> Result<(), Box<dyn std::error::Error>> {
    //for path in std::fs::read_dir("./programs")? {
    //    if let Ok(entry) = path {
    //        if let Some(extension) = entry.path().extension() {
    //            if extension.to_str().unwrap() == "mk" {
    //                let output = run(&entry.path())?;
    //                let file = std::fs::File::open(entry.path().with_extension("expect"))?;
    //                let reader = BufReader::new(file);
    //                let expected: Expected = serde_json::from_reader(reader)?;

    //                assert_eq!(output.status.code().unwrap(), expected.exit_code);
    //            }
    //        }
    //    }
    //}

    Ok(())
}
