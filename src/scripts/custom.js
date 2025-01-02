// Makes the checks table in the main `README.md` interactive through the
// `DataTables` plugin
const createDataTable = async () => {
  const [
    DataTableCSS,
    { 'default': DT },
    { 'default': jQuery },
  ] = await Promise.all([
    import('datatables.net-dt/css/dataTables.dataTables.css'),
    import('datatables.net-dt'),
    import('jquery'),
  ]);

  const table = jQuery('#checks + table').first();

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

// Add here actions that must be run after Docusaurus has made the DOM
// available for further manipulation
export function onRouteDidUpdate({ location, previousLocation }) {
  // Abort if we are still on the same page; the action fires even when
  // navigating within the same page (e.g., between `#` headings)
  if (location.pathname !== previousLocation?.pathname) {

    // Main `README.md`
    if (location.pathname === '/') {
      createDataTable();
    }
  }
}
