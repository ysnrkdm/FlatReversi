//
//  LevelSelectionViewControl.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 10/25/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import UIKit

protocol LevelSelectionViewDelegate: class {
    func finishSelection(level: Level)
}

//@objc(SettingsViewController) class SettingsViewController: UIViewController, UITableViewDataSource, UITableViewDelegate {
class LevelSelectionViewController: UIViewController, UITableViewDataSource, UITableViewDelegate {

    @IBOutlet weak var tableView: UITableView!
    @IBOutlet weak var navbar: UINavigationBar!

    @IBOutlet weak var navbarBack: UIBarButtonItem!

    var idCellBlackPlayer: String?
    var idCellWhitePlayer: String?

    var cells: [(String, [LevelTableCell])]?

    weak var delegate: LevelSelectionViewDelegate? = nil

    override func viewDidLoad() {
        super.viewDidLoad()

        let customStepperCell: UINib = UINib(nibName: "CustomStepperTableViewCell", bundle: nil)
        self.tableView.registerNib(customStepperCell, forCellReuseIdentifier: "stepperCell")

        let lc = LevelController()

        cells = [
            ("Levels", []),
            ("Achievement AIs", [])
            ]

        for level in LevelController().getLevels(true) {
            if lc.isNullAI(level) {
                continue
            } else if lc.isAchievementAI(level) {
                cells![1].1.append(LevelTableCell(tableView: self.tableView, level: level))
            } else {
                cells![0].1.append(LevelTableCell(tableView: self.tableView, level: level))
            }
        }
    }

    override func viewWillAppear(animated: Bool) {
        super.viewWillAppear(animated)

        navbar.frame.size.width = self.view.frame.width
        navbar.sizeToFit()

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

        delegate?.finishSelection(Level(level: 0, levelId: -1, levelTitle: "Human", levelDescr: "Human Player"))

        self.dismissViewControllerAnimated(true, completion: nil)
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

    func tableView(tableView: UITableView, didSelectRowAtIndexPath indexPath: NSIndexPath) {
        //
        delegate?.finishSelection(cells![indexPath.section].1[indexPath.row].level)
        self.dismissViewControllerAnimated(true, completion: nil)
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

    class LevelTableCell: TableCellDefinition {
        var level: Level
        init(tableView: UITableView, level: Level) {
            self.level = level
            super.init(reusableCellId: "kCellBasic", tableView: tableView)
        }

        override func getTableViewCell() -> UITableViewCell {
            var cell = self.tableView.dequeueReusableCellWithIdentifier(reusableCellId) as UITableViewCell
            cell.textLabel.text = level.toString()
            return cell
        }

    }
}
