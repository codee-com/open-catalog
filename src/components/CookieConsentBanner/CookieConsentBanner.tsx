import React, { useEffect } from 'react';
import CookieConsent, { getCookieConsentValue, OPTIONS } from 'react-cookie-consent';
import ReactGA from 'react-ga4';

import './CookieConsentBanner.css';

const CookieConsentBanner = () => {
  const handleAccept = () => {
    if (process.env.NODE_ENV !== 'production') {
      return;
    }
    ReactGA.initialize([{ trackingId: 'G-178ZNT1Z63' }]);
  };

  useEffect(() => {
    if (getCookieConsentValue() !== 'true') {
      return;
    }
    handleAccept();
  }, []);

  return (
    <CookieConsent
      onAccept={handleAccept}
      buttonText='Accept all'
      declineButtonText='Reject'
      enableDeclineButton={true}
      disableStyles={true}
      location={OPTIONS.NONE}
      containerClasses='cookie-consent-banner-container'
      buttonClasses='cookie-consent-banner-button cookie-consent-banner-button-accept'
      declineButtonClasses='cookie-consent-banner-button cookie-consent-banner-button-decline'
    >
      <h3 style={{ color: 'var(--cookie-consent-banner-heading-color)' }}>🍪 We use cookies!</h3>
      <p>
        We only use cookies for analytics to enhance your experience and improve
        our site. Is that okay with you?
      </p>
    </CookieConsent>
  );
};

export default CookieConsentBanner;
