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
    func finishSelectionAppearance(_ appearance: Appearance)
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

        delegate?.finishSelectionAppearance(AppearanceManager.loadAppearanceValue())

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
        var cell: UITableViewCell = UITableViewCell(style: UITableViewCell.CellStyle.subtitle, reuseIdentifier: "MyTestCell")
        cell.textLabel!.text = "Row #\((indexPath as NSIndexPath).row)"

        if let unwrappedCells = cells {
            cell = unwrappedCells[(indexPath as NSIndexPath).section].1[(indexPath as NSIndexPath).row].getTableViewCell()
        }

        return cell
    }

    func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        let row = cells![(indexPath as NSIndexPath).section].1[(indexPath as NSIndexPath).row]
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
}
