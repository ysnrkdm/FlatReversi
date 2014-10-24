//
//  CustomStepperTableViewCell.swift
//  MyFirstSpriteKit
//
//  Created by Kodama Yoshinori on 10/15/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import UIKit

class CustomStepperTableViewCell: UITableViewCell {

    @IBOutlet weak var label: UILabel!
    @IBOutlet weak var stepper: UIStepper!
    @IBOutlet weak var stepLabel: UILabel!
    var valueChangeHookFunction: (Double -> ())?

    override func awakeFromNib() {
        super.awakeFromNib()
        // Initialization code
    }

    override func setSelected(selected: Bool, animated: Bool) {
        super.setSelected(selected, animated: animated)

        // Configure the view for the selected state
    }
    @IBAction func valueChanged(sender: AnyObject) {
        self.stepLabel.text = String(format: "%d", Int(self.stepper.value))
        self.stepLabel.sizeToFit()
        if let fn = valueChangeHookFunction {
            fn(self.stepper.value)
        }
    }

    func configure(labelText: String, stepperValue: Double, stepperMaxValue: Double, stepperMinValue: Double, stepperStepValue:Double, funcToInvokeWhenValueChanged: (Double -> ())) {
        self.label.text = labelText
        self.label.sizeToFit()
        self.stepper.value = stepperValue
        self.stepper.maximumValue = stepperMaxValue
        self.stepper.minimumValue = stepperMinValue
        self.stepper.stepValue = stepperStepValue
        self.stepLabel.text = String(format: "%d", Int(self.stepper.value))
        self.stepLabel.sizeToFit()
        self.valueChangeHookFunction = funcToInvokeWhenValueChanged
    }

    func getDoubleValue() -> Double {
        return self.stepper.value
    }

    func getIntValue() -> Int {
        return Int(self.stepper.value)
    }
}
