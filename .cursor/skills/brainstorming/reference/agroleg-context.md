# Contexto AGROLEG — perguntas socraticas

Adaptacao do protocolo brainstorming para demandas COBOL Bradesco.

## Contexto da demanda

| Contexto | Foco das perguntas |
|---|---|
| **Nova historia Jira** | Iniciativa, mensagens COR/CIR, CC impactados |
| **Correcao / bug** | Sintoma, programa, ambiente, baseline PRD |
| **SICOR 5.12** | Mensagem, tags novas/excluidas, copybook de serializacao |
| **Batch / JCL** | Job, malha upstream/downstream, arquivo de entrada/saida |
| **CICS online** | Transacao, mapa BMS, fluxo de tela |

## Exemplos de boas perguntas

```markdown
### P0 **Mensagem e escopo SICOR**

**Pergunta:** A alteracao e na mensagem COR0005 (inclusao de grupo) ou em outra COR/CIR?

**Por que importa:** define copybooks de serializacao e ordem de tags 5.12

**Opcoes:**
| Opcao | Pros | Contras |
| A | COR0005 apenas | escopo menor |
| B | Multiplas mensagens | mais programas na malha |

---

### P0 **Programa vs copybook**

**Pergunta:** A regra nova entra no programa online (ruca8814) ou so no copybook compartilhado?

**Por que importa:** impacto em CALLers e pre-subida LIBS

---

### P1 **Batch vs online**

**Pergunta:** O processamento e online (CICS) ou batch (JCL)?

**Por que importa:** skills `cics-bms-maps` vs `malha-batch-analise`
```

## Dados minimos antes de Fase 2

- `## Objetivo` compreensivel
- `## Requisito de negocio` com iniciativa e mensagens (se SICOR)
- `## Centros de custo impactados` preenchido ou explicitamente "a confirmar"
- `## Escopo tecnico` com pelo menos um tipo (COBOL/JCL/CICS/BATCH) marcado

Se faltar apos brainstorming, sugerir: `Clarificar requisitos <ID>`.
