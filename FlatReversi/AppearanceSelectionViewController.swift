//
//  AppearanceSelectionViewController.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 12/28/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation
import UIKit

protocol AppearanceSelectionViewDelegate: class {
    func finishSelectionAppearance(appearance: Appearance)
}

class AppearanceSelectionViewController: UIViewController, UINavigationBarDelegate, UITableViewDataSource, UITableViewDelegate {

    @IBOutlet weak var tableView: UITableView!
    @IBOutlet weak var navbar: UINavigationBar!

    @IBOutlet weak var navbarBack: UIBarButtonItem!

    var idCellBlackPlayer: String?
    var idCellWhitePlayer: String?

    var cells: [(String, [TableCellDefinition])]?

    weak var delegate: AppearanceSelectionViewDelegate? = nil

    override func viewDidLoad() {
        super.viewDidLoad()

        navbar.delegate = self;

        cells = [
            ("Appearances", []),
        ]

        for appearance in Appearance.toList() {
            cells![0].1.append(LabelTableCell(tableView: self.tableView, label: appearance.rawValue))
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

    override func supportedInterfaceOrientations() -> UIInterfaceOrientationMask {
        if UIDevice.currentDevice().userInterfaceIdiom == .Phone {
            return UIInterfaceOrientationMask.AllButUpsideDown
        } else {
            return UIInterfaceOrientationMask.All
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

        delegate?.finishSelectionAppearance(AppearanceManager.loadAppearanceValue())

        self.dismissViewControllerAnimated(true, completion: nil)
    }

    func positionForBar(bar: UIBarPositioning) -> UIBarPosition {
        return UIBarPosition.TopAttached
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
        let row = cells![indexPath.section].1[indexPath.row]
        if let ltcrow = row as? LabelTableCell {
            AppearanceManager.persist(Appearance(rawValue: ltcrow.label)!)
            AppearanceManager.load()
            AppearanceManager.resetViews()
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
            return tableView.dequeueReusableCellWithIdentifier(reusableCellId)!
        }
    }

    class LabelTableCell: TableCellDefinition {
        var label: String
        init(tableView: UITableView, label: String) {
            self.label = label
            super.init(reusableCellId: "kCellBasic", tableView: tableView)
        }

        override func getTableViewCell() -> UITableViewCell {
            let cell = self.tableView.dequeueReusableCellWithIdentifier(reusableCellId)!
            cell.textLabel!.text = label
            return cell
        }

    }
}
