export class AudioNotification {
  private static instance: AudioNotification;
  private audio: HTMLAudioElement;
  private isEnabled = true;
  private isUserInteracted = false;

  private constructor() {
    this.audio = new Audio('/audio/ding.mp3');
    this.audio.preload = 'auto';

    const enableAudio = () => {
      this.isUserInteracted = true;
      document.removeEventListener('click', enableAudio);
      document.removeEventListener('keydown', enableAudio);
      document.removeEventListener('scroll', enableAudio);
    };

    document.addEventListener('click', enableAudio, { once: true });
    document.addEventListener('keydown', enableAudio, { once: true });
    document.addEventListener('scroll', enableAudio, { once: true });
  }

  public static getInstance(): AudioNotification {
    if (!AudioNotification.instance) {
      AudioNotification.instance = new AudioNotification();
    }
    return AudioNotification.instance;
  }

  public async play(): Promise<void> {
    if (!this.isEnabled || !this.isUserInteracted) return;

    try {
      this.audio.currentTime = 0;
      await this.audio.play();
    } catch (error) {
      console.warn('Cannot play sound:', error);
      this.showNotification();
    }
  }

  public setEnabled(enabled: boolean): void {
    this.isEnabled = enabled;
  }

  public isAudioEnabled(): boolean {
    return this.isEnabled;
  }

  private showNotification(): void {
    if (document.querySelector('.audio-notification')) return;

    const notification = document.createElement('div');
    notification.className = 'audio-notification';
    notification.style.position = 'fixed';
    notification.style.bottom = '20px';
    notification.style.right = '20px';
    notification.style.padding = '10px 15px';
    notification.style.backgroundColor = '#4a5568';
    notification.style.color = 'white';
    notification.style.borderRadius = '4px';
    notification.style.cursor = 'pointer';
    notification.style.zIndex = '1000';
    notification.style.display = 'flex';
    notification.style.alignItems = 'center';
    notification.style.gap = '8px';
    notification.innerHTML = `
      <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
        <path d="M19 17a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V9a2 2 0 0 1 2-2h10a2 2 0 0 1 2 2v8z"/>
        <path d="M12 19a3 3 0 0 0 3-3V6a3 3 0 0 0-6 0v10a3 3 0 0 0 3 3z"/>
      </svg>
      <span>Nhấn để bật thông báo âm thanh</span>
    `;
    
    notification.onclick = async () => {
      try {
        await this.audio.play();
        this.isUserInteracted = true;
        notification.remove();
      } catch (error) {
        console.error('Cannot play sound:', error);
      }
    };

    document.body.appendChild(notification);
  }
}

export const audioNotification = AudioNotification.getInstance();