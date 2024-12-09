import React from 'react';

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';
import isInternalUrl from '@docusaurus/isInternalUrl';
import IconExternalLink from '@theme/Icon/ExternalLink';
import type {Props} from '@theme/Footer/LinkItem';
import { Icon } from '@iconify/react';
import CookieConsentBanner from '@site/src/components/CookieConsentBanner/CookieConsentBanner';

export default function FooterLinkItem({item}: Props): JSX.Element {
  const {to, href, label, prependBaseUrlToHref, ...props} = item;
  const toUrl = useBaseUrl(to);
  const normalizedHref = useBaseUrl(href, {forcePrependBaseUrl: true});
  const icon = props['icon'] as string;

  if (label == 'Manage cookies') {
    // Render the CookieConsentBanner directly instead of a regular footer link
    // to allow interactive cookie management without navigating away.
    return <CookieConsentBanner />;
  }
  return (
    <Link
      className="footer__link-item"
      {...(href
        ? {
            href: prependBaseUrlToHref ? normalizedHref : href,
          }
        : {
            to: toUrl,
          })}
      {...props}>
      {icon
        ?
          <Icon icon={icon} />
        :
          label
      }
      {!icon && href && !isInternalUrl(href) && <IconExternalLink />}
    </Link>
  );
}
