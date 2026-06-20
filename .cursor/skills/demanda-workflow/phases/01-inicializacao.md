# Fase 1 — Inicializacao + impacto preliminar

**Disparo:** `Implementar AGROLEG-nnn.xml` (Capgemini) ou equivalente Fase 1 Stefanini/7Comm via `demanda-intake-reuniao`.

## Passos

1. Localizar XML em `Cobol/Projetos/**/AGROLEG-nnn/AGROLEG-nnn.xml` ou caminho informado.
2. Criar pasta da demanda se nao existir: `Cobol/Projetos/<Consultoria>/<ID>/`.
3. Copiar README de `Cobol/Projetos/_indice/template-README-demanda.md` se ausente.
4. Sync Jira:
   ```powershell
   powershell -ExecutionPolicy Bypass -File ".cursor/skills/jira-xml-readme-sync/scripts/sync_jira_xml_readme.ps1" -XmlPath "<xml>" -ReadmePath "<readme>" -InPlace
   ```
5. Acionar `agroleg-knowledge` (Momento A — consulta).
6. Preencher README: Objetivo, Requisito de negocio, Centros de custo, Escopo tecnico, Fontes impactados (preliminar), Historias relacionadas.
7. Se notas ou XML forem vagos: acionar `brainstorming` antes de fechar escopo.
8. Se `## Objetivo` ou `## Requisito de negocio` permanecerem incompletos: sugerir `Clarificar requisitos <ID>` (skill `clarification`) — nao listar fontes P1 ate lacunas criticas resolvidas ou usuario aceitar risco.
9. Montar `## Fontes pendentes baixa` (prioridade P1/P2/P3).
10. Se ja existirem fontes na pasta: rodar `readme-dependencias-libs` com `-InPlace`.
11. Atualizar `## Status workflow`: `Analise preliminar concluida — aguardando fontes`.

## Saida

Ver [templates/saidas-obrigatorias.md](../templates/saidas-obrigatorias.md#fase-1--inicializacao).
