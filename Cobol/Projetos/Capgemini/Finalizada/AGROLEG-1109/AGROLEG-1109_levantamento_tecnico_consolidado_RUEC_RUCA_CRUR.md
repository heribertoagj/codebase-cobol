**AGROLEG-1109**

**Levantamento técnico consolidado**

*Análise de impacto e roteiro de alterações*

Organização principal por sistema: RUCA \| RUEC \| CRUR

24/04/2026

# Identificação do documento

| **Demanda** | AGROLEG-1109 |
|:---|----|
| **Documento** | Levantamento técnico consolidado --- análise de impacto e roteiro de alterações |
| **Sistemas impactados** | RUCA, RUEC e CRUR |
| **Objetivo** | Consolidar os levantamentos técnicos relacionados à adequação ao SICOR 5.12, organizando o conteúdo em três blocos principais por sistema. |
| **Versão** | 2.0 |
| **Data** | 24/04/2026 |

Este documento consolida, em linguagem técnica e executiva, os impactos, riscos e encaminhamentos decorrentes da demanda AGROLEG-1109. O conteúdo foi estruturado por sistema --- RUCA, RUEC e CRUR --- para apoiar a leitura gerencial, a validação técnica e o direcionamento das implementações necessárias à adequação ao SICOR 5.12.

# Sumário

- 1\. RUCA

- 2\. RUEC

- 3\. CRUR

- Anexo A. Conclusão consolidada

- Anexo B. Base de referência consolidada

- Anexo C. COR0005 - Análise complementar

- Anexo D. Programas impactados CNPJ alfanumérico

# 1. RUCA {#ruca}

Esta seção consolida o levantamento técnico do sistema RUCA, com foco na adequação da geração da mensagem COR0001 ao SICOR 5.12 e na substituição do modelo legado de sistema de produção pelo grupo estruturado de oito campos previsto no layout vigente.

## 1.1 Objetivo da demanda {#objetivo-da-demanda}

- Adequar a geração da mensagem COR0001 ao padrão SICOR 5.12.

- Substituir o campo consolidado CodSistProdc (13 posições) pelo Grupo_COR0001_SistProdc (8 campos).

## 1.2 Escopo desta etapa {#escopo-desta-etapa}

- Escopo restrito ao sistema RUCA.

- Objetos analisados: programa RUCA8810.

- Objetos analisados: copybook I#RUCA66.

- Objetos analisados: interface de entrada via I#RUEC58/RUEC8800, considerada apenas sob a ótica do impacto no RUCA.

- Objeto de referência contratual: COR0001.XSD v5.12.

## 1.3 Fora do escopo desta etapa {#fora-do-escopo-desta-etapa}

- Não contempla a análise integral do sistema CRUR.

- Não contempla a análise integral do sistema RUEC.

- Não contempla a análise integral do componente RCOR.

## 1.4 Resumo executivo {#resumo-executivo}

- O fluxo do RUCA ainda opera com o modelo legado, baseado no campo único CodSistProdc.

- O fluxo do RUCA ainda opera com o modelo legado, baseado no campo único CodSistProdc.

- Sem a devida adequação no RUCA, a COR0001 permanece sujeita à rejeição por não conformidade com o layout vigente.

- Sem a devida adequação no RUCA, a COR0001 permanece sujeita à rejeição por não conformidade com o layout 5.12.

## 1.5 Diagnóstico técnico {#diagnóstico-técnico}

1\) Situação atual identificada

- O RUCA8810 ainda transfere o retorno consolidado para o campo legado, conforme o mapeamento RUEC58-SIST-PROD -\> RUCA66-COD-SIST-PRODC.

- O fluxo de serialização ainda preserva blocos que precisam ser conciliados com a versão 5.12, a exemplo de certificação, rastreabilidade e energia, conforme o mapeamento de alterações já identificado.

- O fluxo de serialização ainda preserva blocos que precisam ser reconciliados com a versão 5.12, a exemplo de certificação, rastreabilidade e energia, conforme o mapeamento de alterações já identificado.

**2) Estrutura exigida no SICOR 5.12**

- TpAgrctur (1)

- TpIntgcConsc (1)

- TpGraoSemte (1)

- TpIrrgc (2)

- TpCultvExplc (2)

- TpFaseCicloProdc (2)

- TpClima (2)

- TpOtrPratcManj (2)

O RUEC8800 já trata os componentes que compõem o sistema de produção, o que reduz o esforço de origem de dados para a adequação do RUCA.

- O RUEC8800 já trata os componentes que compõem o sistema de produção.

- RUEC58-TIPO-AGROP.

- RUEC58-TIPO-INTEG.

- RUEC58-GRAO-SEMENTE.

- RUEC58-TIPO-IRRIG.

- RUEC58-TIPO-CULTIVO.

- RUEC58-CICLO-PROD.

- RUEC58-CLIMA-PROD.

- Ponto de atenção: o retorno consolidado ainda é encaminhado por meio do campo RUEC58-SIST-PROD.

- Ponto de atenção: o retorno consolidado ainda é encaminhado por meio do campo RUEC58-SIST-PROD.

## 1.6 Impacto por objeto {#impacto-por-objeto}

**RUCA8810 (alto impacto)**

- Adequar o ponto de carga do sistema de produção, eliminando o uso de RUCA66-COD-SIST-PRODC para envio da COR0001 e passando a preencher os oito campos do Grupo_COR0001_SistProdc.

- Adequar a ordem de serialização da WRK-MSGEM-RURAL em conformidade com a COR0001.XSD versão 5.12.

- Revisar e eliminar a serialização de tags excluídas do catálogo 5.12, quando ainda presentes no fluxo atual.

**I#RUCA66 (alto impacto)**

- Refatorar o layout de montagem da COR0001, removendo o campo legado CodSistProdc e incorporando a estrutura do Grupo_COR0001_SistProdc com os oito campos previstos.

- Interface I#RUEC58 / RUEC8800 (impacto médio no RUCA)

Como estratégia recomendada para a etapa restrita ao RUCA, o sistema consolidado deve ser decomposto localmente, quando necessário, e mapeado para o grupo de oito campos.

Como evolução desejável para etapa posterior, fora do escopo atual, o RUEC deverá devolver os oito campos já segregados.

- Como evolução desejável para etapa futura, fora do escopo atual, o RUEC deverá devolver os oito campos já segregados.

## 1.7 Roteiro de alterações {#roteiro-de-alterações}

Etapa 1 - Linha de base: estabelecer a COR0001.XSD v5.12 como contrato de referência e confirmar a cardinalidade e a sequência do Grupo_COR0001_SistProdc.

- Etapa 2 - Copybook de montagem: atualizar o I#RUCA66 para o novo grupo e remover elementos legados não aderentes à versão 5.12.

- Etapa 3 - Lógica de montagem no RUCA8810: substituir o preenchimento legado pelo conjunto de oito campos e adequar a serialização da mensagem à ordem prevista na versão 5.12.

- Etapa 5 - Testes e evidências: gerar a COR0001 com o grupo completo, validar o aceite, executar casos negativos e regressivos e registrar evidências comparativas do payload da mensagem antes e depois da alteração, com validação estrutural contra a XSD v5.12.

- Etapa 5 - Testes e evidências: gerar a COR0001 com o grupo completo, validar o aceite, executar casos negativos e regressivos e registrar evidências de payload da mensagem antes e depois, com validação estrutural contra a XSD v5.12.

## 1.8 Riscos e mitigações {#riscos-e-mitigações}

- Risco: rejeição da COR0001 por ordem incorreta de tags. Mitigação: realizar teste de serialização com comparativo de sequência.

- Risco: divergência entre o RUCA e os fluxos CRUR/RUEC ainda não adequados. Mitigação: registrar esta entrega como parcial e restrita ao RUCA, com planejamento explícito da fase subsequente.

Risco: divergência entre o RUCA e os fluxos CRUR/RUEC ainda não adequados. Mitigação: registrar esta entrega como parcial e restrita ao RUCA, com planejamento explícito de fase subsequente.

## 1.9 Critérios de aceite {#critérios-de-aceite}

- A COR0001 não deve mais enviar o campo CodSistProdc.

- A estrutura da mensagem deve estar aderente à COR0001.XSD v5.12.

- A estrutura da mensagem deve estar aderente à COR0001.XSD versão 5.12.

- Devem ser apresentadas evidências documentadas de testes positivos, negativos e regressivos.

## 1.10 Base de referência {#base-de-referência}

- AGROLEG-1109.xml

- COR0001.XSD v5.12

- Análise de código: RUCA8810 e RUEC8800

# 2. RUEC {#ruec}

Esta seção consolida o levantamento técnico do sistema RUEC, abrangendo a adequação da COR0001, os impactos resumidos das mensagens COR0003R1 e COR0006R1 e a análise detalhada de copybooks, programas e tabelas afetados pela mudança no tratamento do sistema de produção no SICOR 5.12.

## 2.1 Índice do levantamento RUEC {#índice-do-levantamento-ruec}

- Seção 1: análise resumida de impacto --- COR0003R1 e COR0006R1.

- Seção 2: COR0001 --- objetivo, diagnóstico, impacto e roteiro de alterações.

- Seção 3: COR0006R1 --- análise detalhada de copybooks, tabelas e programas.

- Seção 4: COR0003R1 --- análise complementar no RUEC.

- Seção 5: base de referência.

## 2.2 Análise de impacto (resumo) - COR0003R1 e COR0006R1 {#análise-de-impacto-resumo---cor0003r1-e-cor0006r1}

COR0003R1 (SICOR 5.12 --- sistema de produção)

- Escopo da demanda: substituir o CodSistProdc consolidado pelo grupo de oito campos do sistema de produção no layout da mensagem, em conformidade com o catálogo BACEN 5.12, conforme DRN e anexo Jira.

- Impacto esperado no RUEC: revisão dos copybooks de parse da SISMSG, dos pontos de persistência ou repasse que ainda assumem X(013) consolidado e da validação de tamanho e ordem do payload da mensagem diante do novo grupo.

- Risco transversal: consumidores internos ou gravações que ainda mapeiam um único CSIST_PROD deverão evoluir para o modelo decomposto nos pontos em que a COR0003R1 for tratada funcionalmente; o detalhamento permanece condicionado ao aprofundamento em código.

COR0006R1 (SICOR 5.12 --- sistema de produção)

- Centro de impacto: copybook I#RUEC86, responsável pelo parse da COR0006R1; programa RUEC8850, responsável pela rotina de parse e INSERT; e tabelas ou colunas que persistem CSIST_PROD, como RESP_CONTR_RURAL e TCONTR_ENVIO_RECOR.

- Alteração estrutural: substituir RUEC86-COD-SIST-PRODC PIC X(013) pelo grupo de oito campos, preservando as 13 posições totais do bloco, agora com granularidade aderente ao SICOR 5.12.

- Efeito em cadeia: programas que ainda aplicam formatação posicional legada, como 10:02 e 12:02, sobre o campo consolidado, além de demais consumidores de CSIST_PROD, deverão passar a consumir os oito campos persistidos.

- Diretriz: as tabelas impactadas não devem manter apenas o valor consolidado; o sistema de produção deve passar a ser armazenado de forma decomposta, em conformidade com a mensagem.

## 2.3 COR0001 - Objetivo, diagnóstico, impacto e roteiro {#cor0001---objetivo-diagnóstico-impacto-e-roteiro}

Adequar a geração da COR0001 ao SICOR 5.12 no fluxo do RUEC.

- Adequar a geração da COR0001 ao SICOR 5.12 no fluxo do RUEC.

- Substituir o CodSistProdc consolidado pelo Grupo_COR0001_SistProdc, composto por oito campos.

### **Resumo executivo** {#resumo-executivo-1}

- O programa RUEC8810 é responsável pela geração e serialização da COR0001 para envio.

- O layout RUEC66 ainda mantém o campo RUEC66-COD-SIST-PRODC PIC X(013).

- O fluxo atual ainda aplica tratamento posicional sobre o campo consolidado, notadamente nas posições 10:02 e 12:02.

- Nessas condições, o modelo atual não atende ao padrão SICOR 5.12 para o tratamento do sistema de produção.

### **Diagnóstico técnico** {#diagnóstico-técnico-1}

1\) Situação atual identificada

- O RUEC8810 transfere CSIST-PROD para RUEC66-COD-SIST-PRODC.

- O programa normaliza trechos do campo por substring, utilizando as posições 10:02 e 12:02, com preenchimento de \'00\' quando não há valor.

- A montagem da mensagem ainda contempla o bloco COR0001-9 no modelo legado.

### 2) Requisitos do SICOR 5.12 {#requisitos-do-sicor-5.12}

2\) Requisitos do SICOR 5.12

- TpAgrctur (1)

- TpIntgcConsc (1)

- TpGraoSemte (1)

- TpIrrgc (2)

- TpCultvExplc (2)

- TpFaseCicloProdc (2)

- TpClima (2)

- TpOtrPratcManj (2)

<!-- -->

- A COR0001 não deve mais ser enviada com CodSistProdc consolidado.

### Impacto por objeto {#impacto-por-objeto-1}

**RUEC8810 (alto impacto)**

- Eliminar a formatação baseada em substring sobre o campo consolidado.

- Passar a preencher explicitamente os oito campos no bloco correspondente da COR0001.

**I#RUEC66 (alto impacto)**

- Substituir RUEC66-COD-SIST-PRODC PIC X(013) pelo novo Grupo_COR0001_SistProdc.

- Adequar o grupo COR0001-9 ao novo modelo estrutural.

## 2.4 COR0006R1 - Análise detalhada {#cor0006r1---análise-detalhada}

### Premissa da análise

O foco desta análise recai sobre os impactos em copybooks de parse, programas de tratamento e tabelas DB2.

### 1) Impacto em copybooks {#impacto-em-copybooks}

1.1) I#RUEC86 (alto impacto) - copybook de parse funcional da COR0006R1

- Papel: layout da SISMSG consumida pelo RUEC8850.

- Situação atual: o layout ainda contém o campo consolidado RUEC86-COD-SIST-PRODC PIC X(013).

- Impacto: substituir COD-SIST-PRODC pelo grupo de oito campos do sistema de produção, em conformidade com o SICOR 5.12.

<!-- -->

- TpAgrctur PIC X(001)

- TpIntgcConsc PIC X(001)

- TpGraoSemte PIC X(001)

- TpIrrgc PIC X(002)

- TpCultvExplc PIC X(002)

- TpFaseCicloProdc PIC X(002)

- TpClima PIC X(002)

- TpOtrPratcManj PIC X(002)

<!-- -->

- Observação: o tamanho total do grupo permanece em 13 posições, o que reduz o risco de deslocamento físico do bloco.

Correção sugerida no copybook: remover 10 RUEC86-COD-SIST-PRODC PIC X(013) e incluir os oito campos individualizados do sistema de produção.

### 2) Impacto em tabelas {#impacto-em-tabelas}

2.1) DB2PRD.RESP_CONTR_RURAL (alto impacto funcional)

- Coluna impactada: CSIST_PROD.

- A estrutura consolidada atualmente utilizada não atende à diretriz estabelecida para a demanda.

- A persistência deverá ser decomposta em conformidade com a mensagem, contemplando os oito campos.

- Correção sugerida: ajustar a rotina de parse do RUEC8850 para leitura dos oito campos no I#RUEC86 e evoluir o include e a tabela RUECB046 / RESP_CONTR_RURAL para persistir TP_AGRCTUR, TP_INTGC_CONSC, TP_GRAO_SEMTE, TP_IRRGC, TP_CULTV_EXPLC, TP_FASE_CICLO_PRODC, TP_CLIMA e TP_OTR_PRATC_MANJ.

2.2) DB2PRD.TCONTR_ENVIO_RECOR (médio/alto impacto)

- Coluna impactada: CSIST_PROD.

- A estrutura consolidada atualmente utilizada não atende à diretriz estabelecida para a demanda.

- A persistência deverá ser decomposta em conformidade com a mensagem, contemplando os oito campos.

- Correção sugerida: ajustar a rotina de parse do RUEC8850 para leitura dos oito campos no I#RUEC86 e evoluir o include e a tabela RUECB099 / TCONTR_ENVIO_RECOR para persistir os oito campos individualizados do sistema de produção.

### 3) Impacto em programas {#impacto-em-programas}

**3.1) RUEC8850 (alto impacto - crítico)**

- Papel: programa principal de parse da COR0006R1 e de inserção em RESP_CONTR_RURAL.

- Achado: o parse por ocorrência utiliza RUEC86-GRUPO-COR0006R1-OP e realiza MOVE direto do campo legado para a coluna CSIST_PROD de RUECB046.

- Impacto: adequar o parse ao grupo de oito campos do copybook I#RUEC86, substituir o mapeamento para colunas individualizadas do RUECB046 e ajustar o INSERT para gravação explícita dos oito campos.

**3.2) RUEC8820 (médio/alto impacto)**

- Papel: inserir os dados da operação na tabela DB2PRD.TCONTR_ENVIO_RECOR, incluindo atualmente o sistema de produção na coluna consolidada CSIST_PROD.

- Impacto: revisar o include da tabela e o comando SQL de inserção para substituir o armazenamento consolidado pela gravação dos oito campos específicos, ajustar o mapeamento de dados do programa e eliminar a dependência funcional da coluna CSIST_PROD nos trechos de inserção e nas validações associadas ao fluxo.

3.3) RUEC8804 (médio impacto)

- Papel: atuar como orquestrador do retorno; quando a mensagem é COR0006R1, efetua a chamada do RUEC8850.

- Impacto: validar o contrato de chamada após os ajustes internos no RUEC8850 e nos includes DB2.

**3.4) Programas com formatação legada de CSIST_PROD (alto/médio impacto)**

- Programas analisados: RUEC1550, RUEC1590, RUEC1595, RUEC7170, RUEC8808 e RUEC8810.

- Achado recorrente: utilização de substring com padronização posicional, especialmente nas posições 10:02 e 12:02.

- Impacto: revisar o consumo dos oito campos persistidos e eliminar o tratamento posicional do consolidado sempre que CSIST_PROD deixar de ser a fonte funcional.

**3.5) Programas com uso passivo/referencial (baixo impacto)**

- Programas analisados: RUECC890, RUEC0890, RUEC1450, RUEC8818, RUEC8830 e RUEC8831.

- Achado: referência a :RUECB099.CSIST-PROD em host variable, sem transformação funcional relevante.

- Impacto: revisar SELECTs e host variables para consumo dos novos campos decompostos.

**3.6) Programas de consulta de retorno (baixo impacto direto)**

- Programas analisados: RUEC7200, RUEC8200 e RUEC9160.

- Papel: verificar retorno COR0006R1/COR0006E e o respectivo fluxo de exibição.

- Impacto: baixo no tema sistema de produção; o foco permanece concentrado nas rotinas de parse e persistência.

### 4) Recomendação de implementação (COR0006R1) {#recomendação-de-implementação-cor0006r1}

Etapa 1 - Copybook: alterar o I#RUEC86 para a estrutura de oito campos no bloco de sistema de produção.

- Etapa 2 - Parse: ajustar o RUEC8850 para consumir campos individuais e eliminar a dependência direta de RUEC86-COD-SIST-PRODC no processo de parse.

- Etapa 3 - Persistência: evoluir a persistência das tabelas que utilizam CSIST_PROD para o modelo decomposto e ajustar includes DB2 e comandos SQL (INSERT, SELECT e UPDATE) para os oito campos.

- Etapa 4 - Programas legados: revisar RUEC1550, RUEC1590, RUEC1595, RUEC7170, RUEC8808 e RUEC8810 para eliminar comportamento posicional não aderente, quando aplicável.

### 5) Conclusão (COR0006R1) {#conclusão-cor0006r1}

- Centro de impacto principal: I#RUEC86, RUEC8850 e tabelas que persistem CSIST_PROD.

- Risco predominante: permanência de regras legadas baseadas em substring nos programas que ainda consomem CSIST_PROD.

- Diretriz da demanda: as tabelas impactadas não devem persistir apenas o valor consolidado; devem armazenar o sistema de produção decomposto, em conformidade com a mensagem.

## 2.5 COR0003R1 - Análise complementar {#cor0003r1---análise-complementar}

- A demanda AGROLEG-1109 também abrange a COR0003R1 no mesmo eixo de alteração do sistema de produção, com substituição do CodSistProdc pelo grupo de oito campos.

- Esta seção deverá receber o detalhamento por copybook de parse da SISMSG, programas e tabelas do fluxo RUEC, com o mesmo nível de granularidade empregado na análise da COR0006R1, após varredura de código e alinhamento ao DRN-COR0003R1.

- Até a conclusão desse aprofundamento, recomenda-se utilizar o resumo da seção 2.2 como referência de escopo e risco transversal.

## 2.6 Base de referência {#base-de-referência-1}

- AGROLEG-1109.xml

- Análise de código: RUEC7200, RUEC8200, RUEC8804, RUEC8850, RUEC9160, RUECC890, RUEC0890, RUEC1450, RUEC1550, RUEC1590, RUEC1595, RUEC7170, RUEC8808, RUEC8810, RUEC8818, RUEC8820, RUEC8830, RUEC8831, I#RUEC66 e I#RUEC86

# 3. CRUR {#crur}

Esta seção consolida o levantamento técnico do sistema CRUR, abrangendo a adequação da COR0001 ao SICOR 5.12 e as análises complementares das mensagens COR0003R1 e COR0006R1, com ênfase no tratamento funcional executado no CRUR3C6I.

## 3.1 Objetivo da etapa CRUR {#objetivo-da-etapa-crur}

- Adequar a geração da COR0001 ao SICOR 5.12 no fluxo do CRUR.

- Substituir o uso de CodSistProdc (13 posições) pelo Grupo_COR0001_SistProdc, composto por oito campos.

## 3.2 Resumo executivo {#resumo-executivo-2}

- A geração da COR0001 no CRUR é realizada pelo programa CRUR3CY1.

- O fluxo atual ainda monta e serializa o sistema de produção no formato consolidado de 13 posições.

- Nessas condições, o modelo vigente não atende ao SICOR 5.12 para o grupo de sistema de produção.

## 3.3 COR0001 - Diagnóstico técnico {#cor0001---diagnóstico-técnico}

1\) Situação atual identificada

- No CRUR3CY1, o sistema de produção é obtido na rotina 6300-OBTER-SIST-PROD.

- Os componentes de domínio são consultados em tabelas BACEN, notadamente TAGROP, TINTGC, TIGCAO, TCULTR e TCICLO.

- Após as consultas, o programa consolida os componentes em campo único e os transfere para CRURY1-COD-SIST-PRODC.

- A serialização da COR0001 permanece baseada no grupo legado, inclusive com o bloco COR0001-9 estruturado em campo único.

2\) Requisitos do SICOR 5.12

- TpAgrctur (1)

- TpIntgcConsc (1)

- TpGraoSemte (1)

- TpIrrgc (2)

- TpCultvExplc (2)

- TpFaseCicloProdc (2)

- TpClima (2)

- TpOtrPratcManj (2)

## 3.4 COR0001 - Impacto por objeto {#cor0001---impacto-por-objeto}

CRUR3CY1 (alto impacto)

- Eliminar a dependência de CRURY1-COD-SIST-PRODC como fonte de envio.

- Passar a preencher explicitamente os oito campos do grupo no layout da COR0001.

- Adequar a serialização para preservar a ordem e a cardinalidade definidas na XSD v5.12.

- Revisar grupos e campos legados que tenham sido alterados ou removidos na versão 5.12.

Copybook de COR0001 no CRUR (alto impacto)

- Substituir a definição de CodSistProdc pelo Grupo_COR0001_SistProdc.

- Assegurar a correta definição dos tamanhos 1/1/1/2/2/2/2/2.

Domínios e validações (médio/alto impacto)

- Preservar as consultas e validações de domínio BACEN já existentes.

- Redirecionar esse uso para o preenchimento dos campos individuais de saída, sem reconsolidação em X(013).

## 3.5 COR0001 - Roteiro de alterações {#cor0001---roteiro-de-alterações}

Etapa 1 - Linha de base: fixar a COR0001.XSD v5.12 como contrato e confirmar a sequência de tags do bloco de sistema de produção.

- Etapa 2 - Copybook: atualizar o copybook da COR0001 no CRUR para o grupo de oito campos.

- Etapa 3 - Lógica: refatorar a rotina 6300-OBTER-SIST-PROD e os pontos de montagem da mensagem para popular o novo grupo e eliminar MOVE para campo único de envio.

- Etapa 4 - Testes: executar casos positivos, negativos e regressivos do fluxo CRUR de emissão da COR0001 e da persistência associada.

## 3.6 COR0001 - Riscos e mitigações {#cor0001---riscos-e-mitigações}

- Risco: quebra de serialização por ordem incorreta de tags. Mitigação: validar o payload contra a XSD.

- Risco: divergência entre domínio interno e BACEN. Mitigação: manter a validação por tabelas de domínio.

- Risco: impacto em outros blocos da COR0001. Mitigação: executar regressão completa da montagem.

## 3.7 COR0003R1 - Análise complementar {#cor0003r1---análise-complementar-1}

### **Resumo executivo** {#resumo-executivo-3}

- No CRUR, a COR0003R1 é tratada pelo CRUR1O11 por meio da rotina 2390-TRATA-COR0003R1-E.

- O fluxo identificado é predominantemente voltado ao controle e ao armazenamento da mensagem recebida (SISMSG), sem parse funcional detalhado dos campos de negócio.

- Dessa forma, o impacto funcional direto da alteração de layout na COR0003R1 tende a ser baixo a médio no CRUR1O11, com maior atenção para tamanho e compatibilidade do payload da mensagem.

### **Diagnóstico técnico** {#diagnóstico-técnico-2}

- Roteamento da mensagem: quando COD-MSG = \'COR0003R1\', o CRUR1O11 direciona o fluxo para 2390-TRATA-COR0003R1-E.

- Tratamento executado: MOVE de CRURWE11-SISMSG para estrutura de trabalho curta (CRURWW21-REGISTRO), com captura de metadados básicos e persistência de dados de controle e retorno no fluxo CRURB0WD.

- Estrutura utilizada: o copybook CRURWW21 contém apenas cabeçalho e controle --- COD-MSG, NRO-CTRL-IF, CNPJ e data/hora --- sem campos funcionais da COR0003R1.

### **Impacto por objeto** {#impacto-por-objeto-2}

- CRUR1O11 (baixo/médio impacto): manter o roteamento e o controle da COR0003R1, validando se o tamanho do SISMSG recebido permanece compatível com o fluxo de gravação e trilha; revisar apenas se surgir necessidade de interpretar campos de negócio no próprio programa.

- CRURWW21 (baixo impacto): por se tratar de copybook de controle, não representa parse funcional da COR0003R1 e, portanto, não demanda evolução para regra funcional, salvo em caso de ampliação de metadados de monitoração.

### **Ponto de atenção**

Caso a estratégia futura exija consumo funcional da COR0003R1 no CRUR, será necessário identificar ou introduzir um copybook específico para o payload completo, além do CRURWW21, bem como criar uma rotina dedicada de parse.

### **Roteiro objetivo**

- Etapa 1: validar, em homologação, a recepção e a gravação da COR0003R1 sem truncamento.

- Etapa 2: confirmar a monitoração e a rastreabilidade por NRO-CTRL-IF e COD-MSG.

- Etapa 3: deliberar formalmente se o CRUR permanecerá apenas como receptor de controle ou se passará a interpretar campos de negócio.

## 3.8 COR0006R1 - Análise complementar {#cor0006r1---análise-complementar}

### **Resumo executivo** {#resumo-executivo-4}

- No CRUR, a COR0006R1 é recebida pelo CRUR1O11 e interpretada funcionalmente no CRUR3C6I.

- O CRUR1O11 atua como orquestrador do fluxo --- roteamento, trilha e chamada CICS --- enquanto o CRUR3C6I concentra o tratamento funcional da mensagem.

- A COR0006.XSD v5.12 foi utilizada exclusivamente como referência contratual do SICOR 5.12; o sistema não realiza processamento direto de XSD em tempo de execução.

### **Diagnóstico técnico** {#diagnóstico-técnico-3}

- Papel do CRUR1O11: identificar COD-MSG = COR0006R1, executar 2370-TRATA-COR0006R1, gravar a string da mensagem para trilha e chamar 2379-INTERPRETAR-COR0006R1, que aciona o módulo CRUR3C6I via COMMAREA.

- Referência de aderência 5.12: o contrato define o Grupo_COR0006R1_SistProdc com oito campos; entretanto, o código atual ainda se apoia em COD-SIST-PRODC consolidado, o que cria risco de aderência apenas parcial para campos tratados de forma implícita.

- Referência de aderência 5.12: o contrato define o Grupo_COR0006R1_SistProdc com oito campos; entretanto, o código atual ainda se apoia em COD-SIST-PRODC consolidado, o que cria risco de aderência apenas parcial para campos tratados de forma implícita.

### **Impacto por objeto** {#impacto-por-objeto-3}

- CRUR1O11 (baixo/médio impacto): manter o roteamento e a trilha da mensagem, assegurando a integridade do SISMSG encaminhado ao CRUR3C6I; os ajustes tendem a ser pontuais, caso haja mudança de controle ou de tamanho do payload da mensagem.

- CRUR3C6I (alto impacto): principal ponto de impacto funcional da COR0006R1; requer evolução para tratar explicitamente a estrutura do sistema de produção no formato 5.12, reduzindo a dependência da consolidação legada e revisando mapeamentos e validações de domínio associados ao tema.

### **Impacto no copybook**

- Copybook impactado: crurw6r1.cpy, responsável pelo parse funcional da COR0006R1 no CRUR3C6I.

- Situação atual: o copybook ainda define o sistema de produção em campo consolidado, por meio de COR6R1-COD-SIST-PRODC PIC X(013).

- Impacto da adequação ao SICOR 5.12: substituir o campo consolidado por grupo estruturado de oito campos; a COR0006.XSD v5.12 é utilizada apenas como referência contratual, sem leitura direta em execução.

### Tamanho sugerido dos campos a serem criados no copybook

- 10 COR6R1-TP-AGRCTUR PIC X(001). \*\> TpAgrctur

- 10 COR6R1-TP-INTGC-CONSC PIC X(001). \*\> TpIntgcConsc

- 10 COR6R1-TP-GRAO-SEMTE PIC X(001). \*\> TpGraoSemte

- 10 COR6R1-TP-IRRGC PIC X(002). \*\> TpIrrgc

- 10 COR6R1-TP-CULTV-EXPLC PIC X(002). \*\> TpCultvExplc

- 10 COR6R1-TP-FASE-CICLO-PRODC PIC X(002). \*\> TpFaseCicloProdc

- 10 COR6R1-TP-CLIMA PIC X(002). \*\> TpClima

- 10 COR6R1-TP-OTR-PRATC-MANJ PIC X(002). \*\> TpOtrPratcManj

<!-- -->

- O tamanho total do grupo permanece em 13 posições, preservando o comprimento do campo legado.

### **Avaliação de risco técnico**

- Risco estrutural (layout/tamanho): baixo a médio. A soma dos oito campos permanece em 13 posições, reduzindo o risco de deslocamento no parse.

- Risco funcional (regra de negócio): alto. O CRUR3C6I ainda consome COR6R1-COD-SIST-PRODC e realiza decomposição legada; com o novo copybook, o programa deverá passar a consumir explicitamente os oito campos.

### Pontos de atenção

- Assegurar consistência de nomenclatura e tamanho no copybook para evitar falhas em MOVE e nas validações.

- Revisar todas as referências ao campo COR6R1-COD-SIST-PRODC no CRUR3C6I.

- Validar o parse repetitivo do grupo COR6R1-GRUPO-COR0006R1-OP após a substituição estrutural.

### **Roteiro objetivo**

- Etapa 1: mapear, no CRUR3C6I, todos os pontos que utilizam COD-SIST-PRODC e decomposição posicional.

- Etapa 2: ajustar o parse e o mapeamento para a estrutura explícita do grupo de sistema de produção, com oito campos, preservando a compatibilidade com o fluxo atual.

- Etapa 3: validar a recepção fim a fim (CRUR1O11 -\> CRUR3C6I) sem truncamento da SISMSG.

- Etapa 4: executar testes positivos, negativos de domínio e regressivos de gravação em TRESP_PSSOA_BACEN.

## 3.9 Base de referência {#base-de-referência-2}

- AGROLEG-1109.xml

- COR0001.XSD v5.12

- COR0006.XSD v5.12 (somente referência de contrato)

- Análise de código: CRUR3CY1, CRUR1O11, CRUR3C6I e CRURWW21

# Anexo A. Conclusão consolidada {#anexo-a.-conclusão-consolidada}

- Nos três sistemas analisados, a adequação da demanda AGROLEG-1109 converge para a eliminação do uso funcional do CodSistProdc consolidado como estrutura principal de envio, recepção, parse ou persistência.

- No eixo COR0001, os principais pontos de intervenção concentram-se na geração e na serialização das mensagens em RUCA8810, RUEC8810 e CRUR3CY1, bem como na evolução dos respectivos copybooks para suportar o Grupo_COR0001_SistProdc.

- No eixo COR0006R1, os impactos mais relevantes concentram-se no RUEC8850 e no CRUR3C6I, com reflexos em copybooks, mapeamentos de domínio, comandos SQL, persistência e revisão do consumo legado.

- No eixo COR0003R1, o RUEC ainda demanda aprofundamento técnico em código, enquanto o CRUR, no estágio atual, apresenta tratamento predominantemente voltado a controle e rastreabilidade, com impacto funcional direto estimado como baixo a médio.

- Como diretriz geral, a implementação deve priorizar a aderência ao SICOR 5.12, a eliminação de dependências de substring sobre o campo consolidado, a preservação da integridade do payload da mensagem e a formalização de evidências de testes positivos, negativos e regressivos.

# Anexo B. Base de referência consolidada {#anexo-b.-base-de-referência-consolidada}

- AGROLEG-1109.xml

- COR0001.XSD v5.12

- COR0006.XSD v5.12 (referência de contrato)

- RUCA: RUCA8810, I#RUCA66, interface I#RUEC58/RUEC8800

- RUEC: RUEC7200, RUEC8200, RUEC8804, RUEC8850, RUEC9160, RUECC890, RUEC0890, RUEC1450, RUEC1550, RUEC1590, RUEC1595, RUEC7170, RUEC8808, RUEC8810, RUEC8818, RUEC8820, RUEC8830, RUEC8831, I#RUEC66 e I#RUEC86

- CRUR: CRUR3CY1, CRUR1O11, CRUR3C6I, CRURWW21 e crurw6r1.cpy

# Anexo C. COR0005 - Análise complementar {#anexo-c.-cor0005---análise-complementar}

Esta seção complementa o levantamento da AGROLEG-1109 com o mapeamento dos campos NumOrdemDestcOr e Grupo_COR0005_DestcFincmnt no fluxo de geração da COR0005.

Nos artefatos analisados da AGROLEG-1109, o tratamento desses campos foi identificado nos geradores da mensagem COR0005: ruca8814.cbl, ruec8830.cbl e ruec8831.cbl.

O grupo XML Grupo_COR0005_DestcFincmnt é representado no layout COBOL pelo bloco físico COR0005-2. No RUCA, esse bloco está definido em RUCA67-COR0005-2, no copybook i#ruca67.cpy, com marcador {1000}. O campo NumOrdemDestcOr corresponde ao campo COBOL RUCA67-NRO-ORDEM-DEST, PIC X(003), declarado em RUCA67-COR0005-3.

No RUCA, o programa ruca8814.cbl utiliza o copybook I#RUCA67 como área de formatação da COR0005. O processamento move RUCA67-COR0005-2 para o buffer WRK-MSGEM-RURAL, carrega RUCA67-NRO-ORDEM-DEST com o valor fixo \'001\', formata RUCA67-VLR-PROD-OBTD e, na sequência, move RUCA67-COR0005-3 para o mesmo buffer. Desse modo, o grupo é montado em duas etapas: primeiro, o delimitador do grupo em COR0005-2; depois, os campos internos do grupo em COR0005-3.

A carga de VlrProdddObtd no RUCA observa o indicador de nulo WRK-B002-QPROTV-ADQUI. Quando o indicador assume o valor -1, RUCA67-VLR-PROD-OBTD recebe ALL \"\*\". Caso contrário, QPROTV-ADQUI OF RUFIB002 é movido para WRK-S13V99-DRESS, os quatro primeiros bytes de RUCA67-VLR-PROD-OBTD recebem zeros e o valor reformatado WRK-X15-DRESS é gravado nas posições finais do campo.

No RUEC, os programas ruec8830.cbl e ruec8831.cbl reproduzem o mesmo padrão com utilização do copybook I#RUECC5. O bloco RUECC5-COR0005-2 é movido para o buffer WRK-B09A-SIS para representar o Grupo_COR0005_DestcFincmnt; em seguida, RUECC5-NRO-ORDEM-DEST recebe \'001\'. Para VlrProdddObtd, quando WRK-B002-QPROTV-NULL indica nulo, RUECC5-VLR-PROD-OBTD recebe ALL \"\*\"; caso contrário, QPROTV-ADQUI OF RUFIB002 é movido para WRK-SZD17V2 e o campo reformatado WRK-CH19 é transferido para RUECC5-VLR-PROD-OBTD.

Nesse contexto, o copybook atua como layout físico intermediário de montagem da mensagem: os campos são carregados individualmente e, posteriormente, os blocos formatados são concatenados ao buffer de saída. Nos programas analisados, não foi identificada rotina de parse de recebimento para esses campos; o uso observado restringe-se à montagem e ao envio da COR0005.

Conforme a COR0005.XSD v5.12 vinculada à demanda, Grupo_COR0005_DestcFincmnt é opcional, com minOccurs=\'0\' e maxOccurs=\'60\'. O campo NumOrdemDestcOr utiliza o tipo NumOrdemDestc, definido como inteiro com totalDigits=3 e minInclusive=0. O valor fixo \'001\' mostra-se compatível com a tipologia; contudo, o tratamento atual não evidencia suporte a múltiplas ocorrências do grupo até o limite de 60.

Ponto de atenção: o comentário do copybook i#ruca67.cpy ainda indica \[0..50\] para Grupo_COR0005_DestcFincmnt, em divergência com a COR0005.XSD v5.12, que define maxOccurs=\'60\'. Recomenda-se ajustar a documentação e o copybook, bem como validar cenário com múltiplas destinações, caso essa necessidade exista no negócio.

# Anexo D. Programas impactados CNPJ alfanumérico {#anexo-d.-programas-impactados-cnpj-alfanumérico}

Este anexo relaciona os programas e campos identificados como impactados no contexto de CNPJ alfanumérico, preservando a nomenclatura técnica originalmente levantada.

### CRUR3CY1

- WRK-CCPF-CNPJ-BENEF PIC 9(09)

- WRK-CCPF-CNPJ PIC 9(09)

- WRK-CNPJBASE-PROPT-N PIC 9(008)

### CRUR3C6I

- WRK-CPF-CNPJ-14 PIC 9(014)

- WRK-CNPJ-ENT-RESP-OR PIC 9(014)

- WRK-CNPJ-N PIC 9(009)

### RUCA8810

- WRK-NUM-CNPJ-AUX PIC 9(08)

- WRK-FILIAL-CNPJ-AUX PIC 9(04)

- WRK-CTRL-CNPJ-AUX PIC 9(02)

### RUCA8814

- WRK-CCNPJ-INSTC-EXIG PIC +9(08) --- comentado

- WRK-FILIAL-CNPJ-AUX PIC 9(04)

- WRK-CTRL-CNPJ-AUX PIC 9(02)

### RUEC8810

- WRK-CCNPJ-CPF PIC S9(09) COMP-3

- WRK-CFLIAL-CNPJ PIC S9(05) COMP-3

- WRK-CCTRL-CNPJ-CPF PIC S9(02) COMP-3

- WRK-EDIT-CNPJ-R PIC 9(14)

### RUEC8850

- WRK-CNPJ-9 PIC 9(008)

### RUEC8830

- WRK-EDIT-CNPJ-R PIC 9(14)

- WRK-CCNPJ-CPF PIC S9(09) COMP-3

- WRK-CFLIAL-CNPJ PIC S9(05) COMP-3

- WRK-CCTRL-CNPJ-CPF PIC S9(02) COMP-3

### RUEC8831

- WRK-EDIT-CNPJ-R PIC 9(14)

- WRK-CCNPJ-CPF PIC S9(09) COMP-3

- WRK-CFLIAL-CNPJ PIC S9(05) COMP-3

- WRK-CCTRL-CNPJ-CPF PIC S9(02) COMP-3

### RUECC890

- WRK-47-CNPJ-CPF-EMIT PIC 9(014)

- WRK-48-CNPJ-CPF-EMIT PIC 9(014)

### RUEC0890

- WRK-47-CNPJ-CPF-EMIT PIC 9(014)

- WRK-48-CNPJ-CPF-EMIT PIC 9(014)

### RUEC8818

- WRK-EDIT-NCNPJ PIC 9(08)

- WRK-EDIT-FCNPJ PIC 9(04)

- WRK-EDIT-CCNPJ PIC 9(02)

- WRK-EDIT-CNPJ-R PIC 9(14)
