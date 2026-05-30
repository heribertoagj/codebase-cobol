---
name: jira-xml-readme-sync
description: Sincroniza informacoes de historias Jira exportadas em XML para o README da demanda, incluindo o bloco Objetivo a partir do HTML em description. Use ao iniciar demanda AGROLEG, abrir historia, sync Jira ou atualizar chave, titulo, status, responsavel, objetivo e data a partir de arquivo XML.
---

# Jira XML -> README Sync

## Objetivo desta skill
Atualizar o README da demanda usando XML exportado do Jira: secao `## Objetivo` (campo `description`) e bloco `## Dados Jira (sync)`.

## Quando usar
- Ambiente com restricao de seguranca sem acesso direto a API Jira.
- Usuario exporta historia em XML e quer atualizar README automaticamente.

## Entradas
- XML Jira (ex.: `Cobol/Projetos/_jira-xml/AGROLEG-993.xml`)
- README da demanda (ex.: `Cobol/Projetos/Capgemini/AGROLEG-993/README.md`)

## Execucao
1. Gerar bloco Markdown:
   - `powershell -ExecutionPolicy Bypass -File ".cursor/skills/jira-xml-readme-sync/scripts/sync_jira_xml_readme.ps1" -XmlPath "<xml>" -ReadmePath "<readme>"`
2. Atualizar README automaticamente:
   - `powershell -ExecutionPolicy Bypass -File ".cursor/skills/jira-xml-readme-sync/scripts/sync_jira_xml_readme.ps1" -XmlPath "<xml>" -ReadmePath "<readme>" -InPlace`

Com `-InPlace`, o script tambem substitui o conteudo apos `## Objetivo` ate o proximo `##`, usando texto extraido do `<description>` (HTML), cortado antes de `Detalhamento do negocio` e convertido em lista com bullets.

## Regras
- Nao inventar valores ausentes no XML.
- Preservar secoes do README fora de `## Objetivo`, `## Dados Jira (sync)` e do seu conteudo substituido.
- Se a secao nao existir, inserir antes de `## Status`.
- Preencher campos vazios com `Nao informado`.
- Leitura do XML e do README em UTF-8; gravacao do README em UTF-8 sem BOM.
- RSS Jira pode ter texto antes de `<rss>`; o script remove esse prefixo.
- Entidades HTML nao padrao (ex.: `&nbsp;`) sao normalizadas antes do parse.
- Alguns exports trazem `summary` com sequencia incorreta (`U+00C7 U+00C3` + O em vez de `cao`); o script corrige para `cao`.
- Reparo de mojibake (`Repair-MojibakeUtf8`) so corre quando ha padroes como `Ã§`, `Ã£` (UTF-8 lido como Latin-1).

## Formato de saida no README
```markdown
## Dados Jira (sync)
- Chave:
- Titulo:
- Status:
- Responsavel:
- Atualizado em:
```
