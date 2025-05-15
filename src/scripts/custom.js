// Makes the checks table in the main `README.md` interactive through the
// `DataTables` plugin
const createDataTable = async () => {
  const [
    DataTableCSS,
    DataTableButtonsCSS,
    { 'default': DT },
    DataTableButtons,
    { 'default': jQuery },
  ] = await Promise.all([
    import('datatables.net-dt/css/dataTables.dataTables.css'),
    import('datatables.net-buttons-dt/css/buttons.dataTables.css'),
    import('datatables.net-dt'),
    import('datatables.net-buttons/js/dataTables.buttons'),
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
    // Convert the checks table into a DataTable
    let dataTable = table.DataTable({
      // Apply custom sorting logic
      columnDefs: [
        // ID
        { 'targets': 0, 'type': 'checks' },
        // C, Fortran, C++, AutoFix
        { 'targets': [3, 4, 5, 6], 'type': 'ticks' },
        // Disable 'Category' column to be orderable
        { 'orderable': false, 'targets': 2 },
      ],
      // Filters
      layout: {
        topStart: {
          buttons: [
            ...[
              { 'label': 'All checks', 'searchValue': '' },
              { 'label': 'Correctness', 'searchValue': 'correctness' },
              { 'label': 'Modernization', 'searchValue': 'modernization' },
              { 'label': 'Security', 'searchValue': 'security' },
              { 'label': 'Portability', 'searchValue': 'portability' },
              { 'label': 'Optimization', 'searchValue': 'optimization' },
            ].map(({ label, searchValue }) => ({
              'text': label,
              'attr': {
                id: `filter-button-${searchValue}`
              },
              'action': (event, dataTable, node, config) => {
                dataTable.column(2).search(searchValue).draw();
                jQuery(node).addClass('dt-button-clicked');
                jQuery(node).siblings().removeClass('dt-button-clicked');
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
    jQuery('.dt-button').first().addClass('dt-button-clicked');

    // Replace the search bar label with a placeholder
    jQuery('.dt-search label').remove();
    jQuery('.dt-search input').attr('placeholder', 'Filter checks...');
  }
};

const syncDataTableWithURL = async (location) => {
  const { default: jQuery } = await import('jquery');

  const hash = location.hash.toLowerCase().substr(1); // (0) is the `#` part
  // Allow an empty `hash` to go through, since it matches the "All checks"
  // option
  const categoryButton = jQuery(`#filter-button-${hash}`);
  if (categoryButton.length == 1) {
    categoryButton.first().trigger('click');
  }
}

// Add here actions that must be run after Docusaurus has made the DOM
// available for further manipulation
export async function onRouteDidUpdate({ location, previousLocation }) {
  // Main `README.md`
  if (location.pathname === '/') {
    // Omit if we are still on the same page; the action fires even when
    // navigating within the same page (e.g., between `#` headings)
    if (location.pathname !== previousLocation?.pathname) {
      // Wait until the table is loaded since there are dependant actions
      await createDataTable();
    }

    syncDataTableWithURL(location);
  }
}
