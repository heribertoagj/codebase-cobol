# 08 — Copybook e Data Division

## Exclusao hierarquica — BLOQUEANTE

Ao desativar um `01` (ou grupo), comentar **toda a arvore** — nunca deixar `05`/`10` filhos ativos.

Risco de orfaos: subordinados ativos **anexam-se ao 01 anterior**, distorcendo `LENGTH OF` e serializacao (ex.: COR0005).

```cobol
CG2605** EXCLUIDO GRUPO - SICOR 5.12
CG2605* -01 GRUPO-EXCLUIDO.
CG2605*    -05 CAMPO-A                   PIC X(010).
CG2605*    -05 CAMPO-B                   PIC 9(005).
CG2605*    -05 GRUPO-FILHO.
CG2605*       -10 CAMPO-C                PIC X(003).
```

Apos exclusao, validar que nenhum campo excluido permanece referenciado:

- `MOVE` / `COMPUTE`
- `EXEC SQL` SELECT/INTO
- Null indicators
- `LENGTH OF`

## Layout SICOR (COR/CIR)

- Abertura/fechamento explicitos de grupos (`Grupo_...` / `/Grupo_...`).
- Ordem de tags conforme catalogo vigente (5.12 ≠ 5.11).
- Grupo completo quando substituir campo simples (ex.: `SistProdc`).
- Requisito de negocio 5.12: `agroleg-knowledge/initiatives/SICOR-5.12.md`.

## Alteracao em copybook

Seguir cap. 03:

1. Comentar linha/bloco antigo com prefixo e `-01`/`-05`.
2. Criar definicao nova abaixo.
3. **Nao apagar** linhas.

## Checklist (cap. 08)

- [ ] Exclusao comentou arvore completa
- [ ] Verbos/niveis inviaveis em bloco comentado
- [ ] Sem referencia ativa a campo excluido
- [ ] Serializacao salta para proximo bloco valido
- [ ] Ordem SICOR conforme iniciativa/catalogo
