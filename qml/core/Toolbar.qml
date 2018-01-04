import QtQuick 2.0
import QtQuick.Controls 1.3
import "../buttonstyles"

Rectangle {
    id: toolbarRoot
    color: 'light gray';
    width: parent.width
    height: 32+toolbarPadding*2

    property int toolbarPadding: 3

    signal loadView(string name, variant model, var selectedTile, var selectedTileAfter)

    signal toggleMenu()
    signal searchTrigger(bool isSearchActive)

    signal backAction()
    signal forwardAction()
    signal environmentChangeAction(string envType)

    /**
     * actions to display in the toolbar
     * each action is [name, icon, text]
     * When an action is clicked, the
     * actionTriggered signal is emitted.
     */
    property variant actions: []

    /**
     * environments which are available:
     * EnvDevelopment | EnvUat | EnvStage | EnvProd
     */
    property variant environments: []

    property variant pathLinks: []

    property string title: ""

    function setMenuDisplayed(displayed) {
        menuBtn.checked = displayed
    }

    function disableSearch() {
        searchBtn.checked = false
    }

    function setBackActive(isActive) {
        backBtn.enabled = isActive
    }
    function setForwardActive(isActive) {
        fwdBtn.enabled = isActive
    }

    function getIconName(environment) {
        switch (environment) {
        case "EnvDevelopment":
            return "glyphicons-361-bug"
        case "EnvUat":
            return "glyphicons-534-lab"
        case "EnvStage":
            return "glyphicons-140-adjust-alt"
        case "EnvProd":
            return "glyphicons-333-certificate"
        }
        throw "unknown environment: " + environment;
    }

    signal actionTriggered(string name);

    Flow {
        y: toolbarPadding
        x: toolbarPadding
        width: parent.width-x-toolbarPadding*2
        height: parent.height-toolbarPadding*2

        IconButton {
            id: backBtn
            btnText: ''
            iconName: 'glyphicons-217-circle-arrow-left'
            iconSize: 22
            style: menuButton
            width: 30
            height: parent.height
            onClicked: backAction()
        }

        IconButton {
            id: fwdBtn
            btnText: ''
            iconName: 'glyphicons-218-circle-arrow-right'
            iconSize: 22
            style: menuButton
            width: 30
            height: parent.height
            onClicked: forwardAction()
        }

        IconButton {
            btnText: 'home'
            iconName: 'glyphicons-21-home'
            onClicked: loadView("ProjectList.qml", null, null, null)
            style: breadcrumbsButton
            height: parent.height
            checked: pathLinks.length === 0 && title.length === 0
        }
        Repeater {
            model: pathLinks

            Button {
                text: modelData.display
                height: parent.height
                onClicked: loadView(modelData.screen, modelData.model, null, null)
                style: breadcrumbsButton
            }
        }
        ExclusiveGroup { id: tabPositionGroup }
        Button {
            text: title
            height: parent.height
            visible: title.length > 0
            checkable: true
            checked: true
            exclusiveGroup: tabPositionGroup
            style: breadcrumbsButton
        }
        Menu {
            id: envMenu
            MenuItem {
                iconSource: "../../glyphicons-free/" + getIconName("EnvDevelopment")
                text: "Development"
                onTriggered: {
                    envBtn.iconName = getIconName("EnvDevelopment")
                    environmentChangeAction("EnvDevelopment")
                }
                // >= 1 is on purpose: don't offer if we're the active one currently
                visible: toolbarRoot.environments.indexOf("EnvDevelopment") >= 1
            }
            MenuItem {
                iconSource: "../../glyphicons-free/" + getIconName("EnvUat")
                text: "UAT"
                onTriggered: {
                    envBtn.iconName = getIconName("EnvUat")
                    environmentChangeAction("EnvUat")
                }
                // >= 1 is on purpose: don't offer if we're the active one currently
                visible: toolbarRoot.environments.indexOf("EnvUat") >= 1
            }
            MenuItem {
                iconSource: "../../glyphicons-free/" + getIconName("EnvStage")
                text: "Staging"
                onTriggered: {
                    envBtn.iconName = getIconName("EnvStage")
                    environmentChangeAction("EnvStage")
                }
                // >= 1 is on purpose: don't offer if we're the active one currently
                visible: toolbarRoot.environments.indexOf("EnvStage") >= 1
            }
            MenuItem {
                iconSource: "../../glyphicons-free/" + getIconName("EnvProd")
                text: "PROD"
                onTriggered: {
                    envBtn.iconName = getIconName("EnvProd")
                    environmentChangeAction("EnvProd")
                }
                // >= 1 is on purpose: don't offer if we're the active one currently
                visible: toolbarRoot.environments.indexOf("EnvProd") >= 1
            }
        }
        IconButton {
            id: envBtn
            iconName: toolbarRoot.environments.length > 1 ?
                getIconName(toolbarRoot.environments[0]) : getIconName("EnvProd")
            width: 32
            iconX: 6
            height: parent.height
            menu: envMenu
            visible: toolbarRoot.environments.length > 1
        }
        IconButton {
            id: envBtnSingle
            iconName: toolbarRoot.environments.length === 1 ?
                getIconName(toolbarRoot.environments[0]) : getIconName("EnvProd")
            width: 32
            iconX: 9
            height: parent.height
            visible: toolbarRoot.environments.length === 1
            style: Rectangle {}
        }
    }

    Action {
        shortcut: "F1"
        onTriggered: {
            // actions will be undefined from the search screen
            if (actions && actions.length > 0) {
                actionTriggered(actions[0][0])
            }
        }
    }

    // currently we only ever have one action in the toolbar,
    // so this is not necessary, but...
    Action {
        shortcut: "F2"
        onTriggered: {
            if (actions.length > 1) {
                actionTriggered(actions[1][0])
            }
        }
    }

    Flow {
        id: flow
        y: toolbarPadding
        anchors.right: parent.right
        anchors.rightMargin: toolbarPadding
        height: parent.height-toolbarPadding*2
        Repeater {
            id: rightActions
            model: actions
            IconButton {
                iconName: modelData[1]
                btnText: modelData[2]
                onClicked: actionTriggered(modelData[0])
        //      style: normalButtonStyle
                height: flow.height
            }
        }
        IconButton {
            id: searchBtn
            iconName: 'glyphicons-28-search'
            iconSize: 20
            iconX: 5
            onClicked: searchTrigger(searchBtn.checked)
            height: parent.height
            checkable: true
            tooltip: 'Search (ctrl-s)'
        }
        IconButton {
            id: menuBtn
            width: 30
            iconX: 12
            iconName: 'glyphicons-518-option-vertical'
            iconSize: 20
            onClicked: toggleMenu()
            height: parent.height
            style: menuButton
            checkable: true
        }
    }

    Action {
        id: searchAction
        shortcut: "Ctrl+s"
        onTriggered: { searchBtn.checked = true; searchTrigger(searchBtn.checked) }
    }

    Component {
        id: defaultButtonStyle
        DefaultButtonStyle {}
    }
    Component {
        id: normalButtonStyle
        NormalButtonStyle {}
    }
    Component {
        id: breadcrumbsButton
        BreadcrumbsButton {}
    }
    Component {
        id: menuButton
        MenuButton {}
    }
}
