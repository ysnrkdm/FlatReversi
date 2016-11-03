//
//  SettingsViewContoller.swift
//  MyFirstSpriteKit
//
//  Created by Kodama Yoshinori on 10/13/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import UIKit
import Graphene

func LOG(body: String!, function: String = #function, line: Int = #line) {
    let datestr = String(NSDate().description);
    print("[\(datestr) : \(function) @ \(line)] \(body)")
}

class SettingsViewController: UIViewController, UINavigationBarDelegate, UITableViewDataSource, UITableViewDelegate, LevelSelectionViewDelegate {

    @IBOutlet weak var tableView: UITableView!
    @IBOutlet weak var navbar: UINavigationBar!

    var idCellBlackPlayer: String?
    var idCellWhitePlayer: String?

    var cells: [(String, [TableCellDefinition])]?

    var levelSelectionFor: Pieces = Pieces.none

    var gc: GameSettings = GameSettings()

    override func viewDidLoad() {
        super.viewDidLoad()

        navbar.delegate = self;

        let customStepperCell: UINib = UINib(nibName: "CustomStepperTableViewCell", bundle: nil)
        self.tableView.register(customStepperCell, forCellReuseIdentifier: "stepperCell")

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
                SegmentTableCell(tableView: self.tableView, id: "blackPlayerSegment", labelText: "Black player", segmentItems: ["Human", "Computer"], selectedSegmentIndex: blackSSI, targetObject: self, targetSelector: #selector(SettingsViewController.changeSegmentBlackPlayer(_:))),
                SegmentTableCell(tableView: self.tableView, id: "whitePlayerSegment", labelText: "White player", segmentItems: ["Human", "Computer"], selectedSegmentIndex: whiteSSI, targetObject: self, targetSelector: #selector(SettingsViewController.changeSegmentWhitePlayer(_:))),
            ]),
            ("Appearance Settings", [
                SwitchTableCell(tableView: self.tableView, id: "", labelText: "Show Possible Moves", switchOn: gc.showPossibleMoves, targetObject: self, targetSelector: #selector(SettingsViewController.switchShowPossibleMoves(_:))),
                SwitchTableCell(tableView: self.tableView, id: "", labelText: "Show Animation", switchOn: gc.showAnimation, targetObject: self, targetSelector: #selector(SettingsViewController.switchAnimation(_:))),
                DetailSelectorDetailTableCell(tableView: self.tableView, id: "appearance", labelText: "Appearance", detailLabeText: AppearanceManager.loadAppearanceValue().rawValue,
                    funcWhenPressed: {() in
                        NSLog("appearance pressed")
                        self.performSegue(withIdentifier: "appearanceDetailSegue",sender: nil)
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

    override func viewWillAppear(_ animated: Bool) {
        let (sectionIndex, cellIndex) = findTableViewCellById("appearance")
        let ipath = IndexPath(row: cellIndex, section: sectionIndex)

        tableView.deselectRow(at: ipath, animated: false)

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

    override var shouldAutorotate : Bool {
        return false
    }

    override var supportedInterfaceOrientations : UIInterfaceOrientationMask {
        if UIDevice.current.userInterfaceIdiom == .phone {
            return UIInterfaceOrientationMask.allButUpsideDown
        } else {
            return UIInterfaceOrientationMask.all
        }
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Release any cached data, images, etc that aren't in use.
    }

    override var prefersStatusBarHidden : Bool {
        return false
    }

    @IBAction func back(_ sender: AnyObject) {
        NSLog("storing the data and exising from view")
        self.dismiss(animated: true, completion: nil)
    }

    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if segue.identifier == "levelDetailSegue" {
            let lsvc: LevelSelectionViewController = segue.destination as! LevelSelectionViewController
            lsvc.delegate = self
        }
    }

    func position(for bar: UIBarPositioning) -> UIBarPosition {
        return UIBarPosition.topAttached
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
            funcWhenPressed: {() in
                NSLog("black level pressed")
                self.levelSelectionFor = Pieces.black
                self.performSegue(withIdentifier: "levelDetailSegue",sender: nil)
        }), at: cellIndex+1)
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
            funcWhenPressed: {() in
                NSLog("white level pressed")
                self.levelSelectionFor = Pieces.white
                self.performSegue(withIdentifier: "levelDetailSegue",sender: nil)
        }), at: cellIndex+1)

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
                        let ipath = IndexPath(row: cellIndex, section: sectionIndex)
                        tableView.deselectRow(at: ipath, animated: true)
                        tableView.reloadRows(at: [ipath], with: UITableViewRowAnimation.automatic)
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
                        let ipath = IndexPath(row: cellIndex, section: sectionIndex)
                        tableView.deselectRow(at: ipath, animated: true)
                        tableView.reloadRows(at: [ipath], with: UITableViewRowAnimation.automatic)
                    }
                }
            }
        }
    }

    func changeSegmentBlackPlayer(_ sender: AnyObject) {
        let sw: UISegmentedControl = sender as! UISegmentedControl
        NSLog("Switched changeSegmentBlackPlayer! Status: %d", sw.selectedSegmentIndex)
        let cellId = "blackPlayerDifficultySegment"
        switch(sw.selectedSegmentIndex) {
        // Hide difficulty
        case 0:
            let (sectionIndex, cellIndex) = deleteTableViewCellById(cellId)
            if(sectionIndex >= 0) {
                self.tableView.deleteRows(at: [IndexPath(row: cellIndex, section: sectionIndex)], with: UITableViewRowAnimation.automatic)
            }
            gc.blackPlayerComputer = false
            gc.saveToUserDefaults()
        // Show difficulty
        case 1:
            let (sectionIndex, cellIndex) = addBlackPlayerDifficulty()
            if(sectionIndex >= 0) {
                self.tableView.insertRows(at: [IndexPath(row: cellIndex, section: sectionIndex)], with: UITableViewRowAnimation.automatic)
            }
            adjustBlackPlayerDifficultyAppearance()
            gc.blackPlayerComputer = true
            gc.saveToUserDefaults()
        default:
            assertionFailure("Should not reach this code!")
        }
    }

    func changeSegmentWhitePlayer(_ sender: AnyObject) {
        let sw: UISegmentedControl = sender as! UISegmentedControl
        NSLog("Switched changeSegmentWhitePlayer! Status: %d", sw.selectedSegmentIndex)
        let cellId = "whitePlayerDifficultySegment"
        switch(sw.selectedSegmentIndex) {
            // Hide difficulty
        case 0:
            // Search "whitePlayerDifficultySegment" and delete it
            let (sectionIndex, cellIndex) = deleteTableViewCellById(cellId)
            if(sectionIndex >= 0) {
                self.tableView.deleteRows(at: [IndexPath(row: cellIndex, section: sectionIndex)], with: UITableViewRowAnimation.automatic)
            }
            gc.whitePlayerComputer = false
            gc.saveToUserDefaults()
            // Show difficulty
        case 1:
            let (sectionIndex, cellIndex) = addWhitePlayerDifficulty()
            if(sectionIndex >= 0) {
                self.tableView.insertRows(at: [IndexPath(row: cellIndex, section: sectionIndex)], with: UITableViewRowAnimation.automatic)
            }
            adjustWhitePlayerDifficultyAppearance()
            gc.whitePlayerComputer = true
            gc.saveToUserDefaults()
        default:
            assertionFailure("Should not reach this code!")
        }
    }

    func deleteTableViewCellById(_ id: String) -> (Int, Int) {
        let (sectionIndex, cellIndex) = findTableViewCellById(id)
        cells![sectionIndex].1.remove(at: cellIndex)
        return (sectionIndex, cellIndex)
    }

    func findTableViewCellById(_ id: String) -> (Int, Int) {
        if let unwrappedCells = cells {
            var sectionIndex = 0
            for section in unwrappedCells {
                if(section.1.count > 0) {
                    var cellIndex = 0
                    for cell in section.1 {
                        if(cell.id == id) {
                            return (sectionIndex, cellIndex)
                        }
                        cellIndex += 1
                    }
                }
                sectionIndex += 1
            }
        }
        return (-1, -1)
    }

    func switchShowPossibleMoves(_ sender: AnyObject) {
        let sw: UISwitch = sender as! UISwitch
        LOG(body: "Switched switchShowPossibleMoves! Status: \(sw.isOn)")

        gc.showPossibleMoves = sw.isOn
        gc.saveToUserDefaults()
    }

    func switchAnimation(_ sender: AnyObject) {
        let sw: UISwitch = sender as! UISwitch
        LOG(body: "Switched switchAnimation! Status: \(sw.isOn)")
        gc.showAnimation = sw.isOn
        gc.saveToUserDefaults()
    }

    func changeDifficulty(_ value: Double) {
        NSLog("SettingsViewController: Value changed to %d", Int(value))
    }

    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return cells![section].1.count
    }

    func numberOfSections(in tableView: UITableView) -> Int {
        return cells!.count
    }

    func tableView(_ tableView: UITableView, titleForHeaderInSection section: Int) -> String? {
        return cells![section].0
    }

    // Row display. Implementers should *always* try to reuse cells by setting each cell's reuseIdentifier and querying for available reusable cells with dequeueReusableCellWithIdentifier:
    // Cell gets various attributes set automatically based on table (separators) and data source (accessory views, editing controls)

    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        var cell: UITableViewCell = UITableViewCell(style: UITableViewCellStyle.subtitle, reuseIdentifier: "MyTestCell")
        cell.textLabel!.text = "Row #\((indexPath as NSIndexPath).row)"

        if let unwrappedCells = cells {
            cell = unwrappedCells[(indexPath as NSIndexPath).section].1[(indexPath as NSIndexPath).row].getTableViewCell()
        }

        return cell
    }

    func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        if let unwrappedCells = cells {
            let cell = unwrappedCells[(indexPath as NSIndexPath).section].1[(indexPath as NSIndexPath).row]
            if let dcell = cell as? DetailSelectorTableCell {
                dcell.funcWhenPressed()
            } else if let dcell = cell as? DetailSelectorDetailTableCell {
                dcell.funcWhenPressed()
            }
        }
    }

    func finishSelection(_ level: Level) {
        //
        NSLog("Selected level is \(level.levelId), \(level.levelTitle) for \(levelSelectionFor.toString())")
        if(level.levelId < 0) {
            // returned. nothing to do
        } else {
            switch(levelSelectionFor) {
            case .black:
                gc.blackPlayerComputerLevelId = level.levelId
                gc.saveToUserDefaults()
                adjustBlackPlayerDifficultyAppearance()
            case .white:
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
            return tableView.dequeueReusableCell(withIdentifier: reusableCellId)!
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
            let cell = self.tableView.dequeueReusableCell(withIdentifier: reusableCellId)!
            cell.textLabel!.text = labelText
            let items = segmentItems
            let segmentView: UISegmentedControl = UISegmentedControl(items: items)
            segmentView.addTarget(targetObject, action: targetSelector, for: UIControlEvents.valueChanged)
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
            let cell = self.tableView.dequeueReusableCell(withIdentifier: reusableCellId)!
            cell.textLabel!.text = labelText
            let switchView: UISwitch = UISwitch()
            switchView.addTarget(targetObject, action: targetSelector, for: UIControlEvents.touchUpInside)
            switchView.setOn(switchOn, animated: true)
            cell.accessoryView = switchView

            return cell
        }
    }

    class DetailSelectorTableCell: TableCellDefinition {
        var labelText: String
        var funcWhenPressed: (() -> ())
        init(tableView: UITableView, id: String, labelText: String, funcWhenPressed: @escaping (() -> ())) {
            self.labelText = labelText
            self.funcWhenPressed = funcWhenPressed
            super.init(reusableCellId: "kCellSelector", tableView: tableView, id: id)
        }

        override func getTableViewCell() -> UITableViewCell {
            let cell = self.tableView.dequeueReusableCell(withIdentifier: reusableCellId)!
            cell.textLabel?.text = labelText
            return cell
        }
    }

    class DetailSelectorDetailTableCell: TableCellDefinition {
        var labelText: String
        var funcWhenPressed: (() -> ())
        var detailLabeText: String
        init(tableView: UITableView, id: String, labelText: String, detailLabeText: String, funcWhenPressed: @escaping (() -> ())) {
            self.labelText = labelText
            self.detailLabeText = detailLabeText
            self.funcWhenPressed = funcWhenPressed
            super.init(reusableCellId: "kCellSelectorDetail", tableView: tableView, id: id)

        }

        override func getTableViewCell() -> UITableViewCell {
            let cell = self.tableView.dequeueReusableCell(withIdentifier: reusableCellId)!
            cell.textLabel?.text = labelText
            cell.detailTextLabel?.text = detailLabeText
            cell.selectionStyle = UITableViewCellSelectionStyle.gray
            return cell
        }
    }
}
