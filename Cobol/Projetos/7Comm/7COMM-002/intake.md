# Intake 7COMM-002

## 1. Identificacao

- Consultoria: 7Comm
- ID: 7COMM-002
- Titulo: Correcao CNPJ/CGC na tela RUCAT631 (programa RUCA6630)
- Data intake: 2026-06-08
- Referencia externa: N/A (descricao inicial do analista)

## 2. Negocio

- Problema: A tela **RUCAT631** (Demonstracao de Situacao) nao carrega a informacao de **CNPJ/CGC** do mutuario ao consultar operacao de Credito Rural via programa **RUCA6630**.
- Objetivo: Garantir exibicao correta do CNPJ/CGC na linha 8 da RUCAT631 (campo `CGCCPF`) apos consulta na RUCAT630.
- Iniciativa: A confirmar — possivel relacao com **CNPJ alfanumerico** (contexto STEF-001 / 7COMM-001).
- Prazo / criticidade: A levantar

## 3. Escopo tecnico

- Centros de custo: **RUCA**
- Tecnologias: COBOL **IMS** + telas **MFS** (nao CICS/BMS)
- Programa citado: **RUCA6630** (usuario mencionou RUCA66630 — no acervo o member e RUCA6630; confirmar no MF)
- Telas MFS: **RUCAT630** (entrada), **RUCAT631** (saida — campo CGCCPF LTH=17, linha 8)
- Tabelas DB2: `DB2PRD.RUCA_CAD_ROOT` (RUCAB003 — campo `B003_CGCPF1`)
- Segmento IMS: cadastro CAD (RURA020 / DB-CAD)
- Utilitarios: POOL0660 (montagem mensagem IMS), POOL0110 (digito controle)
- Ponto de entrada: section `0070-MONTA-TELA-RUCAT631` / `0080-INSERE-TELA-RUCAT631`

## 4. Contexto operacional

- Fluxo online IMS: usuario informa chave da operacao na RUCAT630 → programa acessa DB2/IMS → monta RUCAT631.
- O que esta quebrado: campo **CGC/CPF** na RUCAT631 nao exibe o CNPJ (demais campos — a confirmar).
- Referencia no acervo: `Capgemini/Finalizada/AGROLEG-001/RUCA/` (ruca6630.cbl, rucat631.mfs) — **somente analise**, nao alterar.
- Programa relacionado com tratamento CNPJ mais recente: **RUCA7951** (`1230-OBTER-CPF-CNPJ`, campos `PIC X`).

## 5. Fontes e evidencias

- Fontes ja baixados localmente: **nao** (acervo AGROLEG-001 disponivel apenas para referencia)
- Fontes P1 a baixar do MF (baseline PRD): ver README `## Fontes pendentes baixa`
- Anexos: N/A

## 6. Regras tecnicas ditas na reuniao

- Padrao Bradesco: comentar linha antiga + criar linha nova (nao excluir linhas).
- Escopo inicial: RUCA6630 + RUCAT631 (RUCAT630 se layout impactado).
- Fora de escopo (preliminar): demais PF keys / programas chamados (RUCA6682, RUCA6650, RUCA6690, etc.) salvo se impacto cruzado confirmado na analise.

## 7. Testes e riscos

- Cenarios: consulta operacao com CNPJ numerico; consulta com CNPJ alfanumerico (se aplicavel); regressao PF keys da RUCAT631.
- Riscos: campos `PIC 9` / `999999999/9999` incompativeis com CNPJ alfa; divergencia layout OUT-TELA631 vs MFS; bridge DB2→IMS (HR03*) desatualizado.

## 8. Notas livres

- Usuario: "analise e correcao da tela RUCA66630 que nao carrega CNPJ na RUCAT631 — COBOL IMS, tela MFS."
- Acervo confirma **RUCA6630** (nao 66630) como transacao da RUCAT631.
- Hipotese preliminar: `CAD-CGCCPF1` (PIC 9(15) COMP-3) → `OUT-CGC-CPF631` (PIC 999999999/9999) nao suporta CNPJ alfanumerico ou dados zerados apos migracao.

## Perguntas em aberto

1. Confirmar member no MF: **RUCA6630** ou existe **RUCA66630**?
2. CNPJ falha para todos os contratos ou apenas CNPJ alfanumerico?
3. Campo **NOME** (linha 8) tambem falha ou somente CGC/CPF?
4. Ambiente de reproducao (TST/HOM/PRD)?
5. Chave de operacao / contrato de teste para homologacao?
6. Alteracao recente em RUCAB003 ou segmento IMS CAD?
7. Relacao formal com iniciativa CNPJ alfanumerico?
