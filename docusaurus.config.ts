import {themes as prismThemes} from 'prism-react-renderer';
import type {Config} from '@docusaurus/types';
import type * as Preset from '@docusaurus/preset-classic';
import remarkGithubAdmonitionsToDirectives from 'remark-github-admonitions-to-directives';

const config: Config = {
  title: 'Code Guidelines for Correctness, Modernization, and Optimization',
  tagline: 'A collaborative effort to consolidate expert knowledge on code guidelines for the correctness, modernization, and optimization of code written in C, C++, and Fortran programming languages',
  favicon: 'img/favicon.ico',

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
            'Glossary/**/*.md',
          ],
          exclude: ['**/external/**'],
          editUrl:
            'https://github.com/codee-com/open-catalog/edit/main/',
          beforeDefaultRemarkPlugins: [
            remarkGithubAdmonitionsToDirectives,
          ],
        },
        theme: {
          customCss: './static/css/custom.css',
        },
        gtag: {
          trackingID: 'G-178ZNT1Z63',
        },
      } satisfies Preset.Options,
    ],
  ],

  themeConfig: {
    navbar: {
      logo: {
        alt: 'Codee Logo',
        src: 'img/logo.png',
        srcDark: 'img/logo-dark.png',
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
        {
          title: 'Community',
          items: [
            {
              label: 'Twitter',
              href: 'https://twitter.com/codee_com',
            },
            {
              label: 'LinkedIn',
              href: 'https://www.linkedin.com/company/codee-com/',
            },
            {
              label: 'GitHub',
              href: 'https://github.com/codee-com',
            },
          ],
        },
      ],
      copyright: `Copyright © ${new Date().getFullYear()} Appentra Solutions, S.L.`,
    },
    prism: {
      theme: prismThemes.github,
      darkTheme: prismThemes.dracula,
      additionalLanguages: ['bash', 'c', 'cpp', 'diff', 'fortran', 'json', 'powershell']
    },
  } satisfies Preset.ThemeConfig,

  plugins: [require.resolve('docusaurus-lunr-search')],
};

export default config;
