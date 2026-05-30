# Intake STEF-001

Preenchido a partir de prompt.txt, pedido.txt e Anotacoes (migracao para padrao STEF-{NNN}).

## 1. Identificacao
- Consultoria: Stefanini
- ID: STEF-001
- Titulo: Alteracao campos CNPJ e FILIAL nos programas GFCT
- Data intake: 2026-05-26
- Referencia externa: Sequencia 2025-0038875-5-121 | Mudanca 000771339

## 2. Negocio
- Problema: Campos CNPJ e FILIAL em formato numerico; necessidade de padronizacao alfanumerica e validacao via BRAD2000
- Objetivo: Ajustar formatos PIC, consistencia de campos, movimentacao -ST e acessos DB2 conforme prompt
- Iniciativa regulatoria: N/A
- Prazo: (nao informado)

## 3. Escopo tecnico
- Centros de custo: GFCT
- Tecnologias: COBOL, JCL, DB2, BATCH
- Programas citados: GFCT7500 (pedido.txt), demais em GFCT/
- Jobs/JCL: GFCT/JCL
- Tabelas DB2: ver prompt.txt secao 6

## 4. Contexto operacional
- Especificacao completa: `prompt.txt`
- Pedido pontual: `pedido.txt` (GFCT7500)
- Pacotes: GFCT002943 (CPY), GFCT002944 (ADESAO)

## 5. Fontes e evidencias
- Fontes locais: GFCT/ (baixado)
- Hexa: M502284 - ste1212

## 6. Regras tecnicas da reuniao/spec
- Ver prompt.txt secoes 1-8 (formatacao, PIC, BRAD2000, movimentacao -ST, DB2)

## 7. Testes e riscos
- Validar BRAD2000 e regressao batch
- Nao alterar campos fora dos padroes CNPJ/FILIAL/CGC/CPF-CNPJ

## 8. Notas livres
- Anotacoes menciona ruec0030 — verificar relacao com escopo
