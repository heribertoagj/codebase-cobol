      ******************************************************************
      *    NOME BOOK : CRURW08V                                        *
      *    DESCRICAO : BOOK DE INTERFACE                               *
      *    DATA      : 18/03/2013                                      *
      *    AUTOR     : DAVI RODRIGUES                                  *
      *    EMPRESA   : CAPGEMINI                                       *
      *    GRUPO     : CAPGEMINI                                       *
      *    TAMANHO   : 1024 BYTES                                      *
      *                                                                *
      ******************************************************************
      * CEMPTO-RURAL.......: CODIGO DO EMPREENDIMENTO RURAL            *
      * CUF................: CODIGO DA UNIDADE FEDERATIVA              *
      * CMUN...............: CODIGO DO MUNICIPIO                       *
      * QPROD-EMPTO-MUN....: QTDE PRODUCAO EMPREENDIMENTO MUNICIPIO    *
      * CUND-MEDD..........: CODIGO UNIDADE DE MEDIDA                  *
      * VPRECO-MIN.........: VALOR DE PRECO MINIMO                     *
      * CEMBAL-RURAL.......: CODIGO EMBALAGEM RURAL                    *
      * QEMBAL-PROD-EMPTO..: QTDE EMBALAGEM PRODUCAO EMPREENDIMENTO    *
      * VSDO-NOTA-PROMS....: VALOR SALDO NOTA PROMISSORIA RURAL        *
      * QTDE-FINAN.........: QUANTIDADE FINANCIADA                     *
      * CVARDE-RURAL-BACEN.: CODIGO DE VARIEDADE RURAL BACEN           *
      * CCLASF-EMPTO-RURAL.: CODIGO CLASSIFICACAO EMPREENDIMENTO RURAL *
      * NUM-CEP-RURAL......: NUMERO CEP                                *
      * COMPL-CEP-RURAL....: COMPLEMENTO CEP                           *
      * NUM-LOGR-RURAL.....: NUMERO LOGRADOURO                         *
      * COMPL-LOGR-RURAL...: COMPLEMENTO LOGRADOURO                    *
      * NM-RZ-SOCIAL-ARMAZ.: NOME RAZAO SOCIAL ARMAZENAMENTO           *
      * CPF-CNPJ-ARMAZ.....: CPF / CNPJ ARMAZENAMENTO                  *
      * CPF-CNPJ-FIL-ARMAZ.: CPF / CNPJ FILIAL ARMAZENAMENTO           *
      * CPF-CNPJ-CTRL-ARMAZ: CPF / CNPJ CONTROLE ARMAZENAMENTO         *
ID5995* TIPO-ARMAZ         : TIPO DE ARMAZEM                           *
ID6205* ELOGDR-ARMAZ-RURAL : ENDERECO ARMAZEM                          *
ID6205* EBAIRO-LOGDR-RURAL : BAIRRO ARMAZEM                            *
      ******************************************************************
      * DATA          AUTOR           DESCRICAO / MANUTENCAO           *
      * 13/04/2016    REINAN LIMA     INCLUSAO DE CAMPOS NUM-CEP-RURAL,*
      *                               COMPL-CEP-RURAL, NUM-LOGR-RURAL, *
      *                               COMPL-LOGR-RURAL,                *
      *                               NM-RZ-SOCIAL-ARMAZ,CPF-CNPJ-ARMAZ*
      *                               CPF-CNPJ-FIL-ARMAZ,              *
      *                               CPF-CNPJ-CTRL-ARMAZ;             *
      *                               INCLUSAO DAS DESCRICOES.         *
      ******************************************************************
      *
           05  CRURW08V-REGISTROS.
               10  CRURW08V-CEMPTO-RURAL           PIC  9(006).
               10  CRURW08V-CUF                    PIC  9(006).
               10  CRURW08V-CMUN                   PIC  9(010).
               10  CRURW08V-QPROD-EMPTO-MUN        PIC  9(007)V9(002).
               10  CRURW08V-CUND-MEDD              PIC  9(003).
               10  CRURW08V-VPRECO-MIN             PIC  9(011)V9(004).
               10  CRURW08V-CEMBAL-RURAL           PIC  9(006).
               10  CRURW08V-QEMBAL-PROD-EMPTO      PIC  9(007)V9(002).
               10  CRURW08V-VSDO-NOTA-PROMS        PIC  9(013)V9(002).
               10  CRURW08V-QTDE-FINAN             PIC  9(013)V9(002).
               10  CRURW08V-CVARDE-RURAL-BACEN     PIC  9(005).
               10  CRURW08V-CCLASF-EMPTO-RURAL     PIC  9(006).
               10  CRURW08V-NUM-CEP-RURAL          PIC  9(005).
               10  CRURW08V-COMPL-CEP-RURAL        PIC  9(003).
               10  CRURW08V-NUM-LOGR-RURAL         PIC  9(007).
               10  CRURW08V-COMPL-LOGR-RURAL       PIC  X(030).
               10  CRURW08V-NM-RZ-SOCIAL-ARMAZ     PIC  X(070).
               10  CRURW08V-CPF-CNPJ-ARMAZ         PIC  9(009).
               10  CRURW08V-CPF-CNPJ-FIL-ARMAZ     PIC  9(004).
               10  CRURW08V-CPF-CNPJ-CTRL-ARMAZ    PIC  9(002).
ID5995         10  CRURW08V-TIPO-ARMAZ             PIC  X(001).
ID6205         10  CRURW08V-ELOGDR-ARMAZ-RURAL     PIC  X(070).
ID6205         10  CRURW08V-EBAIRO-LOGDR-RURAL     PIC  X(040).
               10  FILLER                          PIC  X(677).
