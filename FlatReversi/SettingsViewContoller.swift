//
//  SettingsViewContoller.swift
//  MyFirstSpriteKit
//
//  Created by Kodama Yoshinori on 10/13/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import UIKit

class SettingsViewController: UIViewController, UINavigationBarDelegate, UITableViewDataSource, UITableViewDelegate, LevelSelectionViewDelegate {

    @IBOutlet weak var tableView: UITableView!
    @IBOutlet weak var navbar: UINavigationBar!

    var idCellBlackPlayer: String?
    var idCellWhitePlayer: String?

    var cells: [(String, [TableCellDefinition])]?

    var levelSelectionFor: Pieces = Pieces.None

    var gc: GameSettings = GameSettings()

    override func viewDidLoad() {
        super.viewDidLoad()

        navbar.delegate = self;

        let customStepperCell: UINib = UINib(nibName: "CustomStepperTableViewCell", bundle: nil)
        self.tableView.registerNib(customStepperCell, forCellReuseIdentifier: "stepperCell")

        gc.loadFromUserDefaults()

        // Load game settings and set it to view
        var blackSSI = 0
        if gc.blackPlayerComputer {
            blackSSI = 1
        }
        var whiteSSI = 0
        if gc.whitePlayerComputer {
            whiteSSI = 1
        }

        cells = [
            ("Player Settings", [
                SegmentTableCell(tableView: self.tableView, id: "blackPlayerSegment", labelText: "Black player", segmentItems: ["Human", "Computer"], selectedSegmentIndex: blackSSI, targetObject: self, targetSelector: "changeSegmentBlackPlayer:"),
                SegmentTableCell(tableView: self.tableView, id: "whitePlayerSegment", labelText: "White player", segmentItems: ["Human", "Computer"], selectedSegmentIndex: whiteSSI, targetObject: self, targetSelector: "changeSegmentWhitePlayer:"),
            ]),
            ("Appearance Settings", [
                SwitchTableCell(tableView: self.tableView, id: "", labelText: "Show Possible Moves", switchOn: gc.showPossibleMoves, targetObject: self, targetSelector: "switchShowPossibleMoves:"),
                SwitchTableCell(tableView: self.tableView, id: "", labelText: "Show Animation", switchOn: gc.showAnimation, targetObject: self, targetSelector: "switchAnimation:"),
                DetailSelectorDetailTableCell(tableView: self.tableView, id: "appearance", labelText: "Appearance", detailLabeText: AppearanceManager.loadAppearanceValue().rawValue,
                    funcWhenPressed: {() in
                        NSLog("appearance pressed")
                        self.performSegueWithIdentifier("appearanceDetailSegue",sender: nil)
                })
            ]),
            // FIXME: Implement in future
//            ("Misc", [
//                TableCellDefinition(reusableCellId: "kCellRemoveAds", tableView: self.tableView, id: ""),
//                TableCellDefinition(reusableCellId: "kCellViewRanking", tableView: self.tableView, id: ""),
//            ]),
        ]

        if gc.blackPlayerComputer {
            addBlackPlayerDifficulty()
            adjustBlackPlayerDifficultyAppearance()
        }
        if gc.whitePlayerComputer {
            addWhitePlayerDifficulty()
            adjustWhitePlayerDifficultyAppearance()
        }
    }

    override func viewWillAppear(animated: Bool) {
        let (sectionIndex, cellIndex) = findTableViewCellById("appearance")
        let ipath = NSIndexPath(forRow: cellIndex, inSection: sectionIndex)

        tableView.deselectRowAtIndexPath(ipath, animated: false)

        super.viewWillAppear(animated)

        navbar.frame.size.width = self.view.frame.width
        navbar.sizeToFit()

        gc.loadFromUserDefaults()

        adjustBlackPlayerDifficultyAppearance()
        adjustWhitePlayerDifficultyAppearance()

        AppearanceManager.load()
        AppearanceManager.resetViews()

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

    override func prepareForSegue(segue: UIStoryboardSegue, sender: AnyObject?) {
        if segue.identifier == "levelDetailSegue" {
            let lsvc: LevelSelectionViewController = segue.destinationViewController as LevelSelectionViewController
            lsvc.delegate = self
        }
    }

    func positionForBar(bar: UIBarPositioning) -> UIBarPosition {
        return UIBarPosition.TopAttached
    }

    func addBlackPlayerDifficulty() -> (section: Int, row: Int) {
        let cellId = "blackPlayerDifficultySegment"
        let (sectionIndex, cellIndex) = findTableViewCellById("blackPlayerSegment")

        var labelText = "Black player level"
        let lc: LevelController = LevelController()
        if let lv = lc.getLevelByLevelId(gc.blackPlayerComputerLevelId) {
            labelText = lv.toString()
        }

        cells![sectionIndex].1.insert(DetailSelectorTableCell(tableView: self.tableView, id: cellId, labelText: labelText,
            {() in
                NSLog("black level pressed")
                self.levelSelectionFor = Pieces.Black
                self.performSegueWithIdentifier("levelDetailSegue",sender: nil)
        }), atIndex: cellIndex+1)
        return (sectionIndex, cellIndex+1)
    }

    func addWhitePlayerDifficulty() -> (section: Int, row: Int) {
        let cellId = "whitePlayerDifficultySegment"
        let (sectionIndex, cellIndex) = findTableViewCellById("whitePlayerSegment")

        var labelText = "White player level"
        let lc: LevelController = LevelController()
        if let lv = lc.getLevelByLevelId(gc.whitePlayerComputerLevelId) {
            labelText = lv.toString()
        }

        cells![sectionIndex].1.insert(DetailSelectorTableCell(tableView: self.tableView, id: cellId, labelText: labelText,
            {() in
                NSLog("white level pressed")
                self.levelSelectionFor = Pieces.White
                self.performSegueWithIdentifier("levelDetailSegue",sender: nil)
        }), atIndex: cellIndex+1)

        return (sectionIndex, cellIndex+1)
    }

    func adjustBlackPlayerDifficultyAppearance() {
        if(gc.blackPlayerComputer) {
            let cellId = "blackPlayerDifficultySegment"
            let (sectionIndex, cellIndex) = findTableViewCellById(cellId)
            if sectionIndex >= 0 {
                if let cell = cells![sectionIndex].1[cellIndex] as? DetailSelectorTableCell {
                    let lc: LevelController = LevelController()
                    if let lv = lc.getLevelByLevelId(gc.blackPlayerComputerLevelId) {
                        cell.labelText = lv.toString()
                        cells![sectionIndex].1[cellIndex] = cell
                        let ipath = NSIndexPath(forRow: cellIndex, inSection: sectionIndex)
                        tableView.deselectRowAtIndexPath(ipath, animated: true)
                        tableView.reloadRowsAtIndexPaths([ipath], withRowAnimation: UITableViewRowAnimation.Automatic)
                    }
                }
            }
        }
    }

    func adjustWhitePlayerDifficultyAppearance() {
        if(gc.whitePlayerComputer) {
            let cellId = "whitePlayerDifficultySegment"
            let (sectionIndex, cellIndex) = findTableViewCellById(cellId)
            if sectionIndex >= 0 {
                if let cell = cells![sectionIndex].1[cellIndex] as? DetailSelectorTableCell {
                    let lc: LevelController = LevelController()
                    if let lv = lc.getLevelByLevelId(gc.whitePlayerComputerLevelId) {
                        cell.labelText = lv.toString()
                        cells![sectionIndex].1[cellIndex] = cell
                        let ipath = NSIndexPath(forRow: cellIndex, inSection: sectionIndex)
                        tableView.deselectRowAtIndexPath(ipath, animated: true)
                        tableView.reloadRowsAtIndexPaths([ipath], withRowAnimation: UITableViewRowAnimation.Automatic)
                    }
                }
            }
        }
    }

    func changeSegmentBlackPlayer(sender: AnyObject) {
        let sw: UISegmentedControl = sender as UISegmentedControl
        NSLog("Switched changeSegmentBlackPlayer! Status: %d", sw.selectedSegmentIndex)
        let cellId = "blackPlayerDifficultySegment"
        switch(sw.selectedSegmentIndex) {
        // Hide difficulty
        case 0:
            let (sectionIndex, cellIndex) = deleteTableViewCellById(cellId)
            if(sectionIndex >= 0) {
                self.tableView.deleteRowsAtIndexPaths([NSIndexPath(forRow: cellIndex, inSection: sectionIndex)], withRowAnimation: UITableViewRowAnimation.Automatic)
            }
            gc.blackPlayerComputer = false
            gc.saveToUserDefaults()
        // Show difficulty
        case 1:
            let (sectionIndex, cellIndex) = addBlackPlayerDifficulty()
            if(sectionIndex >= 0) {
                self.tableView.insertRowsAtIndexPaths([NSIndexPath(forRow: cellIndex, inSection: sectionIndex)], withRowAnimation: UITableViewRowAnimation.Automatic)
            }
            adjustBlackPlayerDifficultyAppearance()
            gc.blackPlayerComputer = true
            gc.saveToUserDefaults()
        default:
            assertionFailure("Should not reach this code!")
        }
    }

    func changeSegmentWhitePlayer(sender: AnyObject) {
        let sw: UISegmentedControl = sender as UISegmentedControl
        NSLog("Switched changeSegmentWhitePlayer! Status: %d", sw.selectedSegmentIndex)
        let cellId = "whitePlayerDifficultySegment"
        switch(sw.selectedSegmentIndex) {
            // Hide difficulty
        case 0:
            // Search "whitePlayerDifficultySegment" and delete it
            let (sectionIndex, cellIndex) = deleteTableViewCellById(cellId)
            if(sectionIndex >= 0) {
                self.tableView.deleteRowsAtIndexPaths([NSIndexPath(forRow: cellIndex, inSection: sectionIndex)], withRowAnimation: UITableViewRowAnimation.Automatic)
            }
            gc.whitePlayerComputer = false
            gc.saveToUserDefaults()
            // Show difficulty
        case 1:
            let (sectionIndex, cellIndex) = addWhitePlayerDifficulty()
            if(sectionIndex >= 0) {
                self.tableView.insertRowsAtIndexPaths([NSIndexPath(forRow: cellIndex, inSection: sectionIndex)], withRowAnimation: UITableViewRowAnimation.Automatic)
            }
            adjustWhitePlayerDifficultyAppearance()
            gc.whitePlayerComputer = true
            gc.saveToUserDefaults()
        default:
            assertionFailure("Should not reach this code!")
        }
    }

    func deleteTableViewCellById(id: String) -> (Int, Int) {
        let (sectionIndex, cellIndex) = findTableViewCellById(id)
        cells![sectionIndex].1.removeAtIndex(cellIndex)
        return (sectionIndex, cellIndex)
    }

    func findTableViewCellById(id: String) -> (Int, Int) {
        if let unwrappedCells = cells {
            var sectionIndex = 0
            for section in unwrappedCells {
                if(section.1.count > 0) {
                    var cellIndex = 0
                    for cell in section.1 {
                        if(cell.id == id) {
                            return (sectionIndex, cellIndex)
                        }
                        ++cellIndex
                    }
                }
                ++sectionIndex
            }
        }
        return (-1, -1)
    }

    func switchShowPossibleMoves(sender: AnyObject) {
        let sw: UISwitch = sender as UISwitch
        NSLog("Switched switchShowPossibleMoves! Status: %d", sw.on)

        gc.showPossibleMoves = sw.on
        gc.saveToUserDefaults()
    }

    func switchAnimation(sender: AnyObject) {
        let sw: UISwitch = sender as UISwitch
        NSLog("Switched switchAnimation! Status: %d", sw.on)
        gc.showAnimation = sw.on
        gc.saveToUserDefaults()
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
        cell.textLabel!.text = "Row #\(indexPath.row)"

        if let unwrappedCells = cells {
            cell = unwrappedCells[indexPath.section].1[indexPath.row].getTableViewCell()
        }

        return cell
    }

    func tableView(tableView: UITableView, didSelectRowAtIndexPath indexPath: NSIndexPath) {
        if let unwrappedCells = cells {
            let cell = unwrappedCells[indexPath.section].1[indexPath.row]
            if let dcell = cell as? DetailSelectorTableCell {
                dcell.funcWhenPressed()
            } else if let dcell = cell as? DetailSelectorDetailTableCell {
                dcell.funcWhenPressed()
            }
        }
    }

    func finishSelection(level: Level) {
        //
        NSLog("Selected level is \(level.levelId), \(level.levelTitle) for \(levelSelectionFor.toString())")
        if(level.levelId < 0) {
            // returned. nothing to do
        } else {
            switch(levelSelectionFor) {
            case .Black:
                gc.blackPlayerComputerLevelId = level.levelId
                gc.saveToUserDefaults()
                adjustBlackPlayerDifficultyAppearance()
            case .White:
                gc.whitePlayerComputerLevelId = level.levelId
                gc.saveToUserDefaults()
                adjustWhitePlayerDifficultyAppearance()
            default:
                assertionFailure("Should not reach this code!")
            }
        }
    }

    class TableCellDefinition {
        var reusableCellId: String
        var tableView: UITableView
        var id: String
        init(reusableCellId: String, tableView: UITableView, id: String) {
            self.reusableCellId = reusableCellId
            self.tableView = tableView
            self.id = id
        }

        func getTableViewCell() -> UITableViewCell {
            return tableView.dequeueReusableCellWithIdentifier(reusableCellId) as UITableViewCell
        }
    }

    class SegmentTableCell: TableCellDefinition {
        var labelText: String
        var segmentItems: [String]
        var selectedSegmentIndex: Int
        var targetObject: AnyObject?
        var targetSelector: Selector
        init(tableView: UITableView, id: String, labelText: String, segmentItems: [String], selectedSegmentIndex: Int, targetObject: AnyObject?, targetSelector: Selector) {
            self.labelText = labelText
            self.segmentItems = segmentItems
            self.selectedSegmentIndex = selectedSegmentIndex
            self.targetObject = targetObject
            self.targetSelector = targetSelector
            super.init(reusableCellId: "kCellBasic", tableView: tableView, id: id)
        }

        override func getTableViewCell() -> UITableViewCell {
            var cell = self.tableView.dequeueReusableCellWithIdentifier(reusableCellId) as UITableViewCell
            cell.textLabel!.text = labelText
            let items = segmentItems
            let segmentView: UISegmentedControl = UISegmentedControl(items: items)
            segmentView.addTarget(targetObject, action: targetSelector, forControlEvents: UIControlEvents.ValueChanged)
            if(selectedSegmentIndex >= 0) {
                segmentView.selectedSegmentIndex = selectedSegmentIndex
            }
            cell.accessoryView = segmentView

            return cell
        }
    }

    class SwitchTableCell: TableCellDefinition {
        var labelText: String
        var switchOn: Bool
        var targetObject: AnyObject?
        var targetSelector: Selector
        init(tableView: UITableView, id: String, labelText: String, switchOn: Bool, targetObject: AnyObject?, targetSelector: Selector) {
            self.labelText = labelText
            self.switchOn = switchOn
            self.targetObject = targetObject
            self.targetSelector = targetSelector
            super.init(reusableCellId: "kCellBasic", tableView: tableView, id: id)
        }

        override func getTableViewCell() -> UITableViewCell {
            let cell = self.tableView.dequeueReusableCellWithIdentifier(reusableCellId) as UITableViewCell
            cell.textLabel!.text = labelText
            let switchView: UISwitch = UISwitch()
            switchView.addTarget(targetObject, action: targetSelector, forControlEvents: UIControlEvents.TouchUpInside)
            switchView.setOn(switchOn, animated: true)
            cell.accessoryView = switchView

            return cell
        }
    }

    class DetailSelectorTableCell: TableCellDefinition {
        var labelText: String
        var funcWhenPressed: (() -> ())
        init(tableView: UITableView, id: String, labelText: String, funcWhenPressed: (() -> ())) {
            self.labelText = labelText
            self.funcWhenPressed = funcWhenPressed
            super.init(reusableCellId: "kCellSelector", tableView: tableView, id: id)
        }

        override func getTableViewCell() -> UITableViewCell {
            let cell = self.tableView.dequeueReusableCellWithIdentifier(reusableCellId) as UITableViewCell
            cell.textLabel?.text = labelText
            return cell
        }
    }

    class DetailSelectorDetailTableCell: TableCellDefinition {
        var labelText: String
        var funcWhenPressed: (() -> ())
        var detailLabeText: String
        init(tableView: UITableView, id: String, labelText: String, detailLabeText: String, funcWhenPressed: (() -> ())) {
            self.labelText = labelText
            self.detailLabeText = detailLabeText
            self.funcWhenPressed = funcWhenPressed
            super.init(reusableCellId: "kCellSelectorDetail", tableView: tableView, id: id)

        }

        override func getTableViewCell() -> UITableViewCell {
            let cell = self.tableView.dequeueReusableCellWithIdentifier(reusableCellId) as UITableViewCell
            cell.textLabel?.text = labelText
            cell.detailTextLabel?.text = detailLabeText
            cell.selectionStyle = UITableViewCellSelectionStyle.Gray
            return cell
        }
    }
}
