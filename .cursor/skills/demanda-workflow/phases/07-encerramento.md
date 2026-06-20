# Fase 7 — Encerramento (pos-homologacao)

**Disparo:** `Finalizar <ID>`

**Pre-requisito:** homologacao concluida e implantacao PRD confirmada pelo usuario — padrao cliente (nao encerrar acervo antes).

## Passos

1. Completar README:
   - `## Impacto tecnico (confirmado)`
   - `## Decisoes tecnicas`
   - `## Historias relacionadas`
   - `## Licoes para proximas analises`
   - `## Evidencias`
2. Acionar `agroleg-knowledge` (Momento B — captura): atualizar `modules/<PREFIXO>.md`.
3. Se SICOR 5.12: atualizar `initiatives/SICOR-5.12.md`.
4. Regenerar indice:
   ```powershell
   powershell -ExecutionPolicy Bypass -File ".cursor/skills/agroleg-knowledge/scripts/rebuild_index.ps1" -ProjetosPath "Cobol/Projetos"
   ```
5. Atualizar `Cobol/Projetos/_indice/catalogo-demandas.md`.
6. Mover para `Finalizada/` somente se usuario pedir.
7. Status: `Implantado` / `Finalizada`; `## Status workflow`: `Encerrada`.

## Saida

Ver [templates/saidas-obrigatorias.md](../templates/saidas-obrigatorias.md#fase-7--encerramento).
