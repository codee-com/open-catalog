import React, { useEffect } from 'react';

const ChecksTable = () => {
    useEffect(() => {
        // JQuery won't work during SSR; let's keep these actions exclusive to
        // browser rendering
        if (typeof window === 'undefined') {
            return;
        }

        const createDataTable = async () => {
            const [
                DataTableCSS,
                DataTableButtonsCSS,
                { 'default': DataTable },
                { 'default': DT },
                DataTableButtons,
                { 'default': jQuery },
            ] = await Promise.all([
                import('datatables.net-dt/css/dataTables.dataTables.css'),
                import('datatables.net-buttons-dt/css/buttons.dataTables.css'),
                import('datatables.net-react'),
                import('datatables.net-dt'),
                import('datatables.net-buttons/js/dataTables.buttons'),
                import('jquery'),
            ]);

            DataTable.use(DT);

            // "PWR --> PWD --> RMK" sorting logic
            // (sorted alphabetical by default)
            const idPriority = { 'PWR': 1, 'PWD': 2, 'RMK': 3 };
            const getIdPriority = (id) => idPriority[id.match(/^[A-Z]+/)] || 999;

            jQuery.extend(DT.ext.type.order, {
                'checks-asc': (a, b) => {
                    return getIdPriority(a) - getIdPriority(b);
                },
                'checks-des': (a, b) => {
                    return getIdPriority(b) - getIdPriority(a);
                },
            });

            // "<tickbox> --> <blank>" sorting logic
            // (blanks before ticks by default)
            const emptyString = (str) => str.trim() === "";

            jQuery.extend(DT.ext.type.order, {
                'ticks-asc': (a, b) => {
                    if (a === b) {
                        return 0;
                    }
                    return emptyString(a) ? 1 : -1;
                },
                'ticks-desc': (a, b) => {
                    if (a === b) {
                        return 0;
                    }
                    return emptyString(a) ? -1 : 1;
                }
            });

            // Assuming the checks table is the first one
            const table = jQuery("table").first();

            // If found
            if (table.length > 0) {
                // DataTable styles
                table.addClass("hover row-borders stripe");

                // Convert the checks table into a DataTable
                let dataTable = table.DataTable({
                    // Apply custom sorting logic
                    columnDefs: [
                        // ID
                        { 'targets': 0, 'type': 'checks' },
                        // C, Fortran, C++, AutoFix
                        { 'targets': [3, 4, 5, 6], 'type': 'ticks' },
                    ],
                    // Filters
                    layout: {
                        topStart: {
                            buttons: [
                                ...[
                                    {
                                        'label': 'All checks',
                                        'searchValue': '',
                                    },
                                    {
                                        'label': 'Correctness',
                                        'searchValue': 'correctness',
                                    },
                                    {
                                        'label': 'Modernization',
                                        'searchValue': 'modernization',
                                    },
                                    {
                                        'label': 'Optimization',
                                        'searchValue': 'optimization',
                                    },
                                ].map(({ label, searchValue }) => ({
                                    'text': label,
                                    'action': (event, dataTable, node, config) => {
                                        dataTable.column(2).search(searchValue).draw();
                                        dataTable.buttons().enable();
                                        dataTable.buttons(node).disable();
                                    }
                                })),
                            ]
                        }
                    },
                    ordering: true,
                    paging: false,
                    searching: true,
                });

                // By default, all checks are shown
                dataTable.buttons([0]).disable();
            }
        };

        createDataTable();
    }, []);
};

export default ChecksTable;