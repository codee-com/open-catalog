import {themes as prismThemes} from 'prism-react-renderer';
import type {Config} from '@docusaurus/types';
import type * as Preset from '@docusaurus/preset-classic';
import remarkGithubAdmonitionsToDirectives from 'remark-github-admonitions-to-directives';

const config: Config = {
  title: 'Code Guidelines for Correctness, Modernization, Security, Portability, and Optimization',
  tagline: 'A collaborative effort to consolidate expert knowledge on code guidelines for the correctness, modernization, security, portability, and optimization of code written in C, C++, and Fortran programming languages',
  favicon: 'img/logos/favicon.ico',

  // Set the production url of your site here
  url: 'https://open-catalog.codee.com',
  // Set the /<baseUrl>/ pathname under which your site is served
  // For GitHub pages deployment, it is often '/<projectName>/'
  baseUrl: '/',

  // GitHub pages deployment config.
  organizationName: 'codee-com',
  projectName: 'open-catalog',

  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',

  // Even if you don't use internationalization, you can use this field to set
  // useful metadata like html lang. For example, if your site is Chinese, you
  // may want to replace "en" with "zh-Hans".
  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  presets: [
    [
      'classic',
      {
        docs: {
          path: '.',
          routeBasePath: '/',
          include: [
            'README.md',
            'Checks/**/*.md',
            'Deprecated/**/*.md',
            'Glossary/**/*.md',
            // Standalone pages are stored in `static/pages/**` instead of
            // `src/pages/**` to prevent conflicts between the `mdx-loader` of
            // the `docs` plugin and the `mdx-loader` of the `pages` plugin. If
            // stored in `src/pages/**`, both loaders attempt to process the
            // same Markdown file, resulting in a build failure. This issue
            // arises because the `docs` plugin's `path` is set to the root of
            // the repository rather than a subdirectory. We do this to align
            // the navigation experience between the Open Catalog website and
            // its GitHub repository.
            'static/pages/**/*.md',
          ],
          exclude: ['**/external/**'],
          editUrl:
            'https://github.com/codee-com/open-catalog/edit/main/',
          beforeDefaultRemarkPlugins: [
            remarkGithubAdmonitionsToDirectives,
          ],
          showLastUpdateTime: true,
        },
        theme: {
          customCss: './src/css/custom.css',
        },
      } satisfies Preset.Options,
    ],
  ],

  themeConfig: {
    navbar: {
      logo: {
        alt: 'Codee Logo',
        src: 'img/logos/logo.png',
        srcDark: 'img/logos/logo-dark.png',
      },
      title: 'open catalog',
      items: [
        {
          href: 'https://github.com/codee-com/open-catalog',
          label: 'GitHub',
          position: 'right',
        },
      ],
    },
    docs: {
      sidebar: {
        hideable: true,
      },
    },
    footer: {
      style: 'dark',
      links: [
        {},
        {
          title: 'Community',
          items: [
            {
              label: 'LinkedIn',
              href: 'https://www.linkedin.com/company/codee-com/',
              icon: 'mdi:linkedin'
            },
            {
              label: 'Twitter',
              href: 'https://x.com/codee_com/',
              icon: 'prime:twitter',
            },
            {
              label: 'Youtube',
              href: 'https://www.youtube.com/@codee_com/',
              icon: 'mdi:youtube',
            },
            {
              label: 'GitHub',
              href: 'https://github.com/codee-com/',
              icon: 'mdi:github'
            },
          ],
        },
        {
          title: 'More',
          items: [
            {
              label: 'Codee Website',
              href: 'https://www.codee.com/',
            },
          ]
        },
        {
          title: 'Legal',
          items: [
            {
              label: 'Cookie policy',
              href: '/cookie-policy',
            },
            {
              label: 'Privacy policy',
              href: '/privacy-policy',
            },
            {
              label: 'Manage cookies',
              href: '#',
            },
            {
              label: 'License notice',
              href: '/license-notice',
            },
          ],
        },
      ],
      copyright: `Copyright © ${new Date().getFullYear()} Appentra Solutions, S.L. - Contents licensed under the Apache License, Version 2.0; see <a href="https://github.com/codee-com/open-catalog/blob/main/NOTICE" target="_blank">NOTICE</a> for exceptions`,
    },
    prism: {
      theme: prismThemes.github,
      darkTheme: prismThemes.dracula,
      additionalLanguages: ['bash', 'c', 'cpp', 'diff', 'fortran', 'json', 'powershell']
    },
  } satisfies Preset.ThemeConfig,

  plugins: [
    require.resolve('docusaurus-lunr-search'),
    [
      '@docusaurus/plugin-client-redirects',
      {
        redirects: [
          {
            to: '/Deprecated/PWR010',
            from: '/Checks/PWR010',
          },
          {
            to: '/Deprecated/PWR033',
            from: '/Checks/PWR033',
          },
          {
            to: '/Deprecated/RMK001',
            from: '/Checks/RMK001',
          },
          {
            to: '/Deprecated/RMK002',
            from: '/Checks/RMK002',
          },
          {
            to: '/Deprecated/RMK003',
            from: '/Checks/RMK003',
          },
          {
            to: '/Deprecated/RMK007',
            from: '/Checks/RMK007',
          },
          {
            to: '/Deprecated/RMK008',
            from: '/Checks/RMK008',
          },
        ],
      },
    ],
  ],

  // Executed on client-side (web browser)
  clientModules: [require.resolve('./src/scripts/custom.js')],
};

export default config;
