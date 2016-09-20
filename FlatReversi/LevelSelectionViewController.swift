//
//  LevelSelectionViewController.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 10/25/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import UIKit

protocol LevelSelectionViewDelegate: class {
    func finishSelection(_ level: Level)
}

class LevelSelectionViewController: UIViewController, UINavigationBarDelegate, UITableViewDataSource, UITableViewDelegate {

    @IBOutlet weak var tableView: UITableView!
    @IBOutlet weak var navbar: UINavigationBar!

    @IBOutlet weak var navbarBack: UIBarButtonItem!

    var idCellBlackPlayer: String?
    var idCellWhitePlayer: String?

    var cells: [(String, [TableCellDefinition])]?

    weak var delegate: LevelSelectionViewDelegate? = nil

    override func viewDidLoad() {
        super.viewDidLoad()

        navbar.delegate = self;

        let customStepperCell: UINib = UINib(nibName: "CustomStepperTableViewCell", bundle: nil)
        self.tableView.register(customStepperCell, forCellReuseIdentifier: "stepperCell")

        let lc = LevelController()

        cells = [
            ("Levels", []),
//            ("Achievement AIs", [])
            ]

        var lastRowString = "To be added..."

        for level in LevelController().getLevels(true) {
            if lc.isNullAI(level) {
                if level.levelTitle == "To be added in next version..." {
                    lastRowString = level.levelTitle
                }
                continue
            } else if lc.isAchievementAI(level) {
//                cells![1].1.append(LevelTableCell(tableView: self.tableView, level: level))
            } else {
                cells![0].1.append(LevelTableCell(tableView: self.tableView, level: level))
            }
        }

        let lastrow = LabelTableCell(tableView: self.tableView, label: lastRowString)
        lastrow.getTableViewCell()
        cells![0].1.append(lastrow)
    }

    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)

        navbar.frame.size.width = self.view.frame.width
        navbar.sizeToFit()

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

        delegate?.finishSelection(Level(level: 0, levelId: -1, levelTitle: "Human", levelDescr: "Human Player"))

        self.dismiss(animated: true, completion: nil)
    }

    func position(for bar: UIBarPositioning) -> UIBarPosition {
        return UIBarPosition.topAttached
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
        let row = cells![(indexPath as NSIndexPath).section].1[(indexPath as NSIndexPath).row]
        if let ltcrow = row as? LevelTableCell {
            delegate?.finishSelection(ltcrow.level)
            self.dismiss(animated: true, completion: nil)
        }
    }

    class TableCellDefinition {
        var reusableCellId: String
        var tableView: UITableView
        init(reusableCellId: String, tableView: UITableView) {
            self.reusableCellId = reusableCellId
            self.tableView = tableView
        }

        func getTableViewCell() -> UITableViewCell {
            return tableView.dequeueReusableCell(withIdentifier: reusableCellId)!
        }
    }

    class LabelTableCell: TableCellDefinition {
        var label: String
        init(tableView: UITableView, label: String) {
            self.label = label
            super.init(reusableCellId: "kCellBasic", tableView: tableView)
        }

        override func getTableViewCell() -> UITableViewCell {
            let cell = self.tableView.dequeueReusableCell(withIdentifier: reusableCellId)!
            cell.textLabel!.text = label
            return cell
        }
        
    }

    class LevelTableCell: TableCellDefinition {
        var level: Level
        init(tableView: UITableView, level: Level) {
            self.level = level
            super.init(reusableCellId: "kCellBasic", tableView: tableView)
        }

        override func getTableViewCell() -> UITableViewCell {
            let cell = self.tableView.dequeueReusableCell(withIdentifier: reusableCellId)!
            cell.textLabel!.text = level.toString()
            return cell
        }

    }
}
