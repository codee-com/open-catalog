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

  const table = jQuery('#checks + table').first();

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
