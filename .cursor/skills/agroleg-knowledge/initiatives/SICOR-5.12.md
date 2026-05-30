# Iniciativa SICOR 5.12 (demanda de negocio)

> Epic regulatória: migração do catálogo SICOR 5.11 → 5.12 no ecossistema Crédito Rural Bradesco.
> Este arquivo compõe o acervo `agroleg-knowledge` — **não é skill permanente**.

## Objetivo da iniciativa

Adequar mensageria COR/CIR ao Catálogo de Serviços do SFN v5.12, garantindo conformidade no envio ao BACEN e consistência entre processos batch e online.

## Fontes regulatórias

- Catálogo de Serviços do SFN v5.12 — Volume III
- Catálogo de Serviços do SFN v5.12 — Volume IV
- Catálogo de Serviços do SFN v5.12 — Volume V
- Planilha de alterações: `Cobol/Projetos/Capgemini/Finalizada/AGROLEG-1109/Alteracoes512v511.xls` (ou equivalente no projeto)
- XSD baseline (pasta AGROLEG-1109): CIR1011, COR0001, COR0003, COR0005, COR0006 e variantes `*E`

## Histórias AGROLEG desta iniciativa

| História | Escopo | Status |
|---|---|---|
| AGROLEG-1109 | Levantamento técnico consolidado RUEC/RUCA/CRUR/RCOR | Finalizada |
| AGROLEG-1239 | COR0005 batch — exclusão campos 5.12 (RUEC8831, I#RUECC5) | Em desenvolvimento |
| AGROLEG-1392, 1395, 1396, 1400, 1402, 1403, 1405, 1412, 1418, 1424, 1446, 1595 | Subtarefas COR0005 online | A levantar |
| AGROLEG-1243 | RCOR* — impacto a confirmar | Em andamento |

Ver também: `index.md` e `modules/RUEC.md`, `modules/RUCA.md`, `modules/RCOR.md`.

## Mensagens com impacto direto na 5.12

| Mensagem | Inclusões | Exclusões | Campos movimentados |
|---|---|---|---|
| CIR1011 | 1 | 0 | 1 movimentação |
| COR0001 | 11 | 10 | 78 |
| COR0003R1 | 11 | 11 | 72 |
| COR0005 | 0 | 10 | 57 |
| COR0006R1 | 10 | 1 | 20 |

## Mapeamento Bradesco (confirmado / em curso)

| Mensagem | Programa / copybook | Módulo | História |
|---|---|---|---|
| COR0005 | RUEC8831 (montador) | RUEC | AGROLEG-1239 |
| COR0005 | I#RUECC5 (layout) | RUEC | AGROLEG-1239 |
| COR0005 | RUEC0895 (batch liberação — sem alteração prevista) | RUEC | AGROLEG-1239 |
| COR0005 | Processos online RUCA (prorrogação, mestres, produtividade, desclassificação) | RUCA | subtarefas 1239 |
| COR0001, COR0006 | RCOR* (vários) | RCOR | AGROLEG-1109, 1243 |

## Mudanças funcionais chave (regulatório)

### 1) Grupo SistProdc substitui CodSistProdc

Nas mensagens impactadas, `CodSistProdc` foi removido e entrou o grupo:

- `Grupo_*_SistProdc`
- `TpAgrctur`, `TpIntgcConsc`, `TpGraoSemte`, `TpIrrgc`, `TpCultvExplc`, `TpFaseCicloProdc`, `TpClima`, `TpOtrPratcManj`
- fechamento `/_SistProdc`

Regra prática: mapear o grupo completo com cardinalidade obrigatória quando exigida — não apenas um subcampo.

### 2) Inclusão de NumOrdemDestcRenegc

Campo opcional em mensagens de crédito rural impactadas (ex.: COR0001, COR0003R1).

### 3) Exclusões de certificação/rastreabilidade

Removidos em mensagens impactadas, incluindo:

- `CodCertifcOrgncoEmpnmnt`, `CodRastlddEmpnmnt`
- grupos `CertifcOrgnco`, `RastlddEmpnmnt`
- COR0005 também: `PercEnerRenovvlEmpnmnt`, `IndrConfcAlertSustldd`

Regra prática Bradesco (AGROLEG-1239): retirar da **mensageria**; campos podem **permanecer em telas** RUCA/RUEC/CRUR.

### 4) Reordenação massiva de tags

Não assumir sequência da 5.11. Revisar tabelas de parser/serializador.

## Tipologias importantes (5.12)

| Campo | Formato | Tamanho | Regex / observação |
|---|---|---|---|
| TpAgrctur | alfanumérico | 1 | `[0-9]{1}` |
| TpIntgcConsc | alfanumérico | 1 | `[0-9]{1}` |
| TpGraoSemte | alfanumérico | 1 | `[0-9]{1}` |
| TpIrrgc | alfanumérico | 2 | `[0-9]{2}` |
| CodDocFis | alfanumérico | 1..50 | |
| IdentdInfSeggc | alfanumérico | 1..50 | |

## Checklist de conformidade 5.12

Usar na análise e pré-subida quando README indicar iniciativa SICOR 5.12:

- [ ] Mensagem alvo está na lista de impacto da 5.12
- [ ] Campos excluídos removidos do mapeamento e da serialização
- [ ] Grupo `SistProdc` implementado completo quando aplicável
- [ ] Cardinalidade obrigatória/opcional respeitada
- [ ] Tipologia (tamanho/regex/formato) validada
- [ ] Sequência de tags atualizada para 5.12
- [ ] XSD correspondente consumido na versão 5.12
- [ ] Exclusão em copybook: hierarquia completa comentada (ver `bradesco-quality-code` regra #10)
- [ ] Teste negativo: campo obrigatório ausente e formato inválido

## Como reportar achados

```text
BLOQUEANTE SICOR 5.12: [mensagem/campo]
Desvio: [o que está diferente da 5.12]
Risco: [rejeição BACEN, perda de informação, parse incorreto]
Correção recomendada: [ajuste objetivo]
```

## Lições acumuladas (Bradesco)

- COR0005 batch: prefixo de rastreio Capgemini `CGaamm*` (ex.: `CG2605*`); verbos comentados inviáveis (`-IF`, `-MOVE`) — Hexavision.
- Ordem tags 5.12 ≠ 5.11 — validar no copybook e no fluxo de montagem, não só no catálogo PDF.
- Impacto COR0005 online espalhado em subtarefas — consultar `index.md` antes de analisar história isolada.

## Integração com skills permanentes

| Necessidade | Onde |
|---|---|
| O quê mudou na 5.12 | Este arquivo |
| Onde no Bradesco | `modules/*.md` + README da AGROLEG |
| Estilo layout COR/CIR | `bradesco-style` |
| Quality copybook/comentários | `bradesco-quality-code` |
| Pipeline demanda | `demanda-workflow`, `demanda-pre-subida` |
