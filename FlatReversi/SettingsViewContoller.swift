//
//  SettingsViewContoller.swift
//  MyFirstSpriteKit
//
//  Created by Kodama Yoshinori on 10/13/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import UIKit

//@objc(SettingsViewController) class SettingsViewController: UIViewController, UITableViewDataSource, UITableViewDelegate {
class SettingsViewController: UIViewController, UITableViewDataSource, UITableViewDelegate {

    @IBOutlet weak var tableView: UITableView!
    @IBOutlet weak var navbar: UINavigationBar!

    var idCellBlackPlayer: String?
    var idCellWhitePlayer: String?

    var gameSettings: GameSettings?

    var cells: [(String, [TableCellDefinition])]?

    override func viewDidLoad() {
        super.viewDidLoad()

        let customStepperCell: UINib = UINib(nibName: "CustomStepperTableViewCell", bundle: nil)
        self.tableView.registerNib(customStepperCell, forCellReuseIdentifier: "stepperCell")

        cells = [
            ("Player Settings", [
                SegmentTableCell(tableView: self.tableView, labelText: "Black player", segmentItems: ["Human", "Computer"], targetObject: self, targetSelector: "changeSegmentBlackPlayer:"),
                SegmentTableCell(tableView: self.tableView, labelText: "White player", segmentItems: ["Human", "Computer"], targetObject: self, targetSelector: "changeSegmentWhitePlayer:"),
                StepperTableCell(tableView: self.tableView, labelText: "Difficulty", stepperValue: 1, stepperMaxValue: 10, stepperMinValue: 1, stepperStepValue: 1, funcToInvokeWhenValueChanged: changeDifficulty)

            ]),
            ("Appearance Settings", [
                SwitchTableCell(tableView: self.tableView, labelText: "Show Possible Moves", targetObject: self, targetSelector: "switchShowPossibleMoves:"),
                SwitchTableCell(tableView: self.tableView, labelText: "Show Animation", targetObject: self, targetSelector: "switchAnimation:")
            ]),
            ("Misc", [
                TableCellDefinition(reusableCellId: "kCellRemoveAds", tableView: self.tableView),
                TableCellDefinition(reusableCellId: "kCellViewRanking", tableView: self.tableView),
            ]),
        ]
    }

    override func viewWillAppear(animated: Bool) {
        super.viewWillAppear(animated)

        navbar.frame.size.width = self.view.frame.width
        navbar.sizeToFit()

        gameSettings?.loadFromUserDefaults()

        NSLog("%f - %f", self.view.frame.width.native, navbar.frame.size.width.native)
    }

    override func shouldAutorotate() -> Bool {
        return false
    }

    override func supportedInterfaceOrientations() -> Int {
        if UIDevice.currentDevice().userInterfaceIdiom == .Phone {
            return Int(UIInterfaceOrientationMask.AllButUpsideDown.rawValue)
        } else {
            return Int(UIInterfaceOrientationMask.All.rawValue)
        }
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Release any cached data, images, etc that aren't in use.
    }

    override func prefersStatusBarHidden() -> Bool {
        return false
    }

    @IBAction func back(sender: AnyObject) {
        NSLog("storing the data and exising from view")
        self.dismissViewControllerAnimated(true, completion: nil)
    }

    func changeSegmentBlackPlayer(sender: AnyObject) {
        let sw: UISegmentedControl = sender as UISegmentedControl
        NSLog("Switched changeSegmentBlackPlayer! Status: %d", sw.selectedSegmentIndex)
        switch(sw.selectedSegmentIndex) {
        case 0:
            cells![0].1.removeAtIndex(1)
            self.tableView.deleteRowsAtIndexPaths([NSIndexPath(forRow: 1, inSection: 0)], withRowAnimation: UITableViewRowAnimation.Automatic)
        case 1:
            cells![0].1.insert(StepperTableCell(tableView: self.tableView, labelText: "Black player level", stepperValue: 1, stepperMaxValue: 10, stepperMinValue: 1, stepperStepValue: 1, funcToInvokeWhenValueChanged: changeDifficulty), atIndex: 1)
            self.tableView.insertRowsAtIndexPaths([NSIndexPath(forRow: 1, inSection: 0)], withRowAnimation: UITableViewRowAnimation.Automatic)
        default:
            assertionFailure("Should not reach this code!")
        }
    }

    func changeSegmentWhitePlayer(sender: AnyObject) {
        let sw: UISegmentedControl = sender as UISegmentedControl
        NSLog("Switched changeSegmentWhitePlayer! Status: %d", sw.selectedSegmentIndex)
    }

    func switchShowPossibleMoves(sender: AnyObject) {
        let sw: UISwitch = sender as UISwitch
        NSLog("Switched switchShowPossibleMoves! Status: %d", sw.on)
    }

    func switchAnimation(sender: AnyObject) {
        let sw: UISwitch = sender as UISwitch
        NSLog("Switched switchAnimation! Status: %d", sw.on)
    }

    func changeDifficulty(value: Double) {
        NSLog("SettingsViewController: Value changed to %d", Int(value))
    }

    func tableView(tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return cells![section].1.count
    }

    func numberOfSectionsInTableView(tableView: UITableView) -> Int {
        return cells!.count
    }

    func tableView(tableView: UITableView, titleForHeaderInSection section: Int) -> String? {
        return cells![section].0
    }

    // Row display. Implementers should *always* try to reuse cells by setting each cell's reuseIdentifier and querying for available reusable cells with dequeueReusableCellWithIdentifier:
    // Cell gets various attributes set automatically based on table (separators) and data source (accessory views, editing controls)

    func tableView(tableView: UITableView, cellForRowAtIndexPath indexPath: NSIndexPath) -> UITableViewCell {
        var cell: UITableViewCell = UITableViewCell(style: UITableViewCellStyle.Subtitle, reuseIdentifier: "MyTestCell")
        cell.textLabel.text = "Row #\(indexPath.row)"

        if let unwrappedCells = cells {
            cell = unwrappedCells[indexPath.section].1[indexPath.row].getTableViewCell()
        }

        return cell
    }

    class TableCellDefinition {
        var reusableCellId: String
        var tableView: UITableView
        init(reusableCellId: String, tableView: UITableView) {
            self.reusableCellId = reusableCellId
            self.tableView = tableView
        }

        func getTableViewCell() -> UITableViewCell {
            return tableView.dequeueReusableCellWithIdentifier(reusableCellId) as UITableViewCell
        }
    }

    class SegmentTableCell: TableCellDefinition {
        var labelText: String
        var segmentItems: [String]
        var targetObject: AnyObject?
        var targetSelector: Selector
        init(tableView: UITableView, labelText: String, segmentItems: [String], targetObject: AnyObject?, targetSelector: Selector) {
            self.labelText = labelText
            self.segmentItems = segmentItems
            self.targetObject = targetObject
            self.targetSelector = targetSelector
            super.init(reusableCellId: "kCellBasic", tableView: tableView)
        }

        override func getTableViewCell() -> UITableViewCell {
            var cell = self.tableView.dequeueReusableCellWithIdentifier(reusableCellId) as UITableViewCell
            cell.textLabel.text = labelText
            let items = segmentItems
            let segmentView: UISegmentedControl = UISegmentedControl(items: items)
            segmentView.addTarget(targetObject, action: targetSelector, forControlEvents: UIControlEvents.ValueChanged)
            cell.accessoryView = segmentView

            return cell
        }
    }

    class StepperTableCell: TableCellDefinition {
        var labelText: String
        var stepperValue: Double
        var stepperMaxValue: Double
        var stepperMinValue: Double
        var stepperStepValue: Double
        var funcToInvokeWhenValueChanged: (Double -> ())
        init(tableView: UITableView, labelText: String, stepperValue: Double, stepperMaxValue: Double, stepperMinValue: Double, stepperStepValue:Double, funcToInvokeWhenValueChanged: (Double -> ())) {
            self.labelText = labelText
            self.stepperValue = stepperValue
            self.stepperMaxValue = stepperMaxValue
            self.stepperMinValue = stepperMinValue
            self.stepperStepValue = stepperStepValue
            self.funcToInvokeWhenValueChanged = funcToInvokeWhenValueChanged
            super.init(reusableCellId: "stepperCell", tableView: tableView)
        }

        override func getTableViewCell() -> UITableViewCell {
            let stepperCell:CustomStepperTableViewCell = self.tableView.dequeueReusableCellWithIdentifier(reusableCellId) as CustomStepperTableViewCell
            stepperCell.configure(labelText, stepperValue: stepperValue, stepperMaxValue: stepperMaxValue, stepperMinValue: stepperMinValue, stepperStepValue: stepperStepValue, funcToInvokeWhenValueChanged: funcToInvokeWhenValueChanged)

            return stepperCell
        }
    }

    class SwitchTableCell: TableCellDefinition {
        var labelText: String
        var targetObject: AnyObject?
        var targetSelector: Selector
        init(tableView: UITableView, labelText: String, targetObject: AnyObject?, targetSelector: Selector) {
            self.labelText = labelText
            self.targetObject = targetObject
            self.targetSelector = targetSelector
            super.init(reusableCellId: "kCellBasic", tableView: tableView)
        }

        override func getTableViewCell() -> UITableViewCell {
            let cell = self.tableView.dequeueReusableCellWithIdentifier(reusableCellId) as UITableViewCell
            cell.textLabel.text = labelText
            let switchView: UISwitch = UISwitch()
            switchView.addTarget(targetObject, action: targetSelector, forControlEvents: UIControlEvents.TouchUpInside)
            cell.accessoryView = switchView

            return cell
        }
    }
}
