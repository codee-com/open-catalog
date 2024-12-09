import React, { useEffect, useState } from 'react';
import CookieConsent, { getCookieConsentValue, OPTIONS, VISIBLE_OPTIONS } from 'react-cookie-consent';
import ReactGA from 'react-ga4';
import Link from '@docusaurus/Link';

import './CookieConsentBanner.css';

const CookieConsentBannerImpl = () => {
  const cookieName: string = 'CookieConsent';
  const googleAnalyticsTrackingId: string = 'G-178ZNT1Z63';
  // Store the banner state in memory to maintain consistency across route
  // changes. For example, if the banner is visible, ensure it remains visible
  // when navigating between pages.
  const isMinimizedValueInMemoryKey: string = 'CookieConsentIsMinimized';

  const getIsMinimizedValueFromMemory = (): undefined | boolean => {
    const isMinimizedValueInMemory: null | string =
      window.sessionStorage.getItem(isMinimizedValueInMemoryKey);
    if (isMinimizedValueInMemory === null) {
      return undefined;
    }
    return JSON.parse(isMinimizedValueInMemory) === true;
  };
  const setIsMinimizedValueInMemory = (value: boolean): void => {
    window.sessionStorage.setItem(isMinimizedValueInMemoryKey, JSON.stringify(value));
  };
  const initializeGoogleAnalytics = (): void => {
    ReactGA.initialize([{ trackingId: googleAnalyticsTrackingId }]);
  };
  const resetGoogleAnalytics = (): void => {
    ReactGA.reset();
  };
  const getCookieValue = (): undefined | boolean => {
    const cookieValue: undefined | string = getCookieConsentValue(cookieName);
    if (cookieValue === undefined) {
      return undefined;
    }
    return cookieValue === 'true';
  };

  const [isMinimized, setIsMinimized] = useState(() => {
    const isMinimizedValueInMemory: undefined | boolean = getIsMinimizedValueFromMemory();
    if (isMinimizedValueInMemory === undefined) {
      // If `isMinimizedValueInMemory` is undefined, we decide whether to show
      // the banner by checking if the value of the cookie is also undefined. If
      // the cookie is undefined, it means it is the first time the user visits
      // the website; therefore, prompt the banner
      return getCookieValue() !== undefined;
    }
    return isMinimizedValueInMemory;
  });

  const handleAccept = () => {
    initializeGoogleAnalytics();
    setIsMinimized(true);
  };
  const handleDecline = () => {
    resetGoogleAnalytics();
    setIsMinimized(true);
  };
  const handleExpand = () => {
    setIsMinimized(false);
  };

  useEffect(() => {
    setIsMinimizedValueInMemory(isMinimized);
  }, [isMinimized]);
  useEffect(() => {
    if (getCookieValue() === true) {
      initializeGoogleAnalytics();
    }
  }, []);

  return (
    <>
      {!isMinimized && (
        <CookieConsent
          cookieName={cookieName}

          buttonText='Accept'
          declineButtonText='Decline'

          onAccept={handleAccept}
          onDecline={handleDecline}
          enableDeclineButton={true}
          acceptOnScroll={false}

          location={OPTIONS.NONE}
          visible={VISIBLE_OPTIONS.SHOW}

          disableStyles={true}

          containerClasses='cookie-consent-banner-container'
          buttonWrapperClasses='cookie-consent-banner-buttons'
          buttonClasses='cookie-consent-banner-button cookie-consent-banner-button-accept'
          declineButtonClasses='cookie-consent-banner-button cookie-consent-banner-button-decline'
        >
          <h3 style={{ color: 'var(--cookie-consent-banner-heading-color)' }}>
            🍪 We use cookies!
          </h3>
          <p>
            We only use cookies to enhance your experience and improve our site. Is
            that okay with you?
          </p>
          <p>
            <Link
              to='/cookie-policy'
              style={{
                color: 'var(--cookie-consent-banner-heading-color)',
                textDecoration: 'underline',
              }}
            >
              Read more
            </Link>
          </p>
        </CookieConsent>
      )}
      <Link to='#' className='footer__link-item' onClick={(e) => {
        // Prevent the default anchor behavior to avoid scrolling the page to
        // the top when the link is clicked.
        e.preventDefault();
        handleExpand();
      }}>Manage cookies</Link>
    </>
  );
};

// Wrapper over the actual `CookieConsentBanner` to make sure it only renders
// in the client. This helps avoiding errors with Server-Side Rendering (SSR)!
const CookieConsentBanner = () => {
  const [isMounted, setIsMounted] = useState(false);
  useEffect(() => {
    setIsMounted(true);
  }, []);
  return isMounted ? <CookieConsentBannerImpl /> : null;
}

export default CookieConsentBanner;
