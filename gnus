好的，配置 Gnus 来收发 iCloud 邮件是完全可行的，但相比 Gmail 等服务器会稍微复杂一些，主要是因为 Apple 的安全要求。

核心在于：你必须为 Gnus 创建一个“应用专用密码”，而不是使用你的 iCloud 账户密码。


------

配置 Gnus 使用 iCloud 邮件

请按照以下步骤操作：

第 1 步：创建应用专用密码

1. 确保你的 iCloud 账户已开启双重认证（Two-Factor Authentication）。这是创建应用专用密码的前提。

2. 访问 Apple ID 管理页面： https://appleid.apple.com(https://appleid.apple.com) 并登录。

3. 进入“安全”部分。

4. 找到“应用专用密码”选项。

5. 点击“生成密码...”，并为其提供一个描述（例如 Emacs Gnus）。

6. 记录下生成的密码（格式类似 xxxx-xxxx-xxxx-xxxx）。这个密码只会显示一次，你将无法再次查看它。这个密码就是你接下来在 Gnus 配置中需要填写的密码。

第 2 步：Gnus 配置代码

将以下配置添加到你的 Emacs 配置文件（如 ~/.emacs.d/init.el 或 ~/.gnus.el）中。

请务必将 your_apple_id@icloud.com 替换为你的完整 iCloud 邮箱地址，并将 your-app-specific-password 替换为你上一步生成的应用专用密码。

;; -*- lexical-binding: t; -*-

(require 'gnus)
(require 'smtpmail)
(require 'starttls) ; 确保支持 STARTTLS

;; --- 设置发送邮件 (SMTP) ---
;; 方法 1: 使用 STARTTLS (端口 587) - 通常更可靠
(setq smtpmail-smtp-server "smtp.mail.me.com"
      smtpmail-smtp-service 587 ; iCloud 的提交端口
      smtpmail-auth-credentials "(smtp.mail.me.com 587 your_apple_id@icloud.com your-app-specific-password)"
      smtpmail-starttls-credentials '(("smtp.mail.me.com" 587 nil nil)) ; 启用 STARTTLS
      smtpmail-auth-supported '(plain login) ; 支持的认证机制
      message-send-mail-function 'smtpmail-send-it ; 使用 smtpmail 发送
      )

;; --- 设置接收邮件 (IMAP) ---
(setq gnus-select-method
      '(nnimap "icloud" ; 给这个连接起个名字
              (nnimap-address "imap.mail.me.com") ; iCloud IMAP 服务器
              (nnimap-server-port 993) ; SSL 端口
              (nnimap-stream ssl) ; 使用 SSL 加密
              (nnimap-authinfo-file "~/.authinfo.gpg") ; 推荐使用 .authinfo 文件，更安全
       ))

;; --- 通用设置 ---
(setq user-full-name "Your Name" ; 替换为你的名字
      user-mail-address "your_apple_id@icloud.com") ; 替换为你的 iCloud 邮箱


第 3 步（推荐）：使用 .authinfo 文件存储密码

将密码明文放在配置文件中不安全。更安全的方法是使用 ~/.authinfo.gpg（一个用GPG加密的文件）。

1. 创建或编辑文件：创建或打开文件 ~/.authinfo（注意没有 .gpg 后缀）。

2. 添加一行内容：

machine imap.mail.me.com login your_apple_id@icloud.com password your-app-specific-password port 993
machine smtp.mail.me.com login your_apple_id@icloud.com password your-app-specific-password port 587


同样，替换邮箱地址和密码。

3. 加密文件：在终端中执行：

gpg --encrypt --recipient your-email@example.com ~/.authinfo


（--recipient 后跟你的GPG密钥ID或关联的邮箱，用于加密此文件。如果还没有GPG密钥，需要先创建一个）。

4. 删除明文文件：加密后会生成 ~/.authinfo.gpg，现在可以安全地删除 ~/.authinfo 文件。

5. 修改 Gnus 配置：如果你使用了 .authinfo.gpg 文件，可以从配置中删除 smtpmail-auth-credentials 这一行。Gnus 和 smtpmail 会自动从该文件中读取认证信息。


------

使用流程

1. 重启 Emacs 或重新加载你的配置文件（例如 M-x eval-buffer）。

2. 启动 Gnus：M-x gnus。

3. 获取并订阅组（邮件夹）：

  ◦ 首次运行时，Gnus 会尝试连接服务器。

  ◦ 在 Group buffer 中，按 A A (或 M-x gnus-group-get-new-news) 来从服务器获取所有可用的邮件夹（如 INBOX, Sent, Drafts 等）。

  ◦ 将光标移动到一个邮件夹上，按 u 来订阅它。订阅后，它才会在列表中保持可见。

4. 阅读邮件：

  ◦ 将光标移动到 INBOX 上，按 RET (回车) 来打开它。

  ◦ 重要提示：默认情况下，Gnus 可能只显示未读邮件。如果你想看到所有邮件，在 Group buffer 中选择 INBOX 时，使用 C-u RET（即先按 Ctrl+u，再按回车），然后输入一个很大的数字（如 1000）作为要获取的邮件数量。

常见问题 & 排错

• 认证失败：99% 的问题都是密码错误。

  ◦ 确保你使用的是 16 字符的应用专用密码，而不是你的 Apple ID 密码。

  ◦ 确保密码格式正确（没有多余的空格）。

• 无法发送邮件：

  ◦ 检查 smtpmail-smtp-service 端口（应是 587）。

  ◦ 检查 smtpmail-starttls-credentials 设置是否正确。

• 无法连接 IMAP：

  ◦ 检查防火墙是否允许连接到 imap.mail.me.com:993。

  ◦ 确认 nnimap-stream 设置为 ssl。

按照以上步骤操作，你应该就能成功在 Emacs Gnus 中享受收发 iCloud 邮件的乐趣了。如果遇到问题，请仔细检查密码和端口设置。
