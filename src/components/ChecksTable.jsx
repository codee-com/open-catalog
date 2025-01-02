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
                { 'default': DataTable },
                { 'default': DT },
                { 'default': jQuery },
            ] = await Promise.all([
                import('datatables.net-dt/css/dataTables.dataTables.css'),
                import('datatables.net-react'),
                import('datatables.net-dt'),
                import('jquery'),
            ]);

            DataTable.use(DT);

            // Assuming the checks table is the first one
            const table = jQuery("table").first();

            // If found
            if (table.length > 0) {
                // DataTable styles
                table.addClass("hover row-borders stripe");

                // Convert the checks table into a DataTable
                let dataTable = table.DataTable({
                    ordering: true,
                    paging: false,
                    searching: true,
                });
            }
        };

        createDataTable();
    }, []);
};

export default ChecksTable;
