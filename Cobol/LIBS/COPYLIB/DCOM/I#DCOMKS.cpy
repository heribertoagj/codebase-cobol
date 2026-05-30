      ******************************************************************
      * SISTEMA : DCOM - DESCONTO COMERCIAL                            *
      * ARQUIVO : LAYOUT DO RETORNO DAS SOLICITACOES DAS NOVAS         *
      *           OPERACOES DO OPEM BANKING                            *
      * AUTOR   : MILTON NOGUEIRA                                      *
      * DDS     : PAULO ROBERTO                                        *
      * EMPRESA : BRQ SOLUTION'S                                       *
      * DATA    : SET/2021                                             *
      * TAMANHO : 0264 BYTES                                           *
      ******************************************************************
      * CAMPO                | DESCRICAO                               *
      *----------------------+-----------------------------------------*
      * OPBKS-BRANDCODE      | CODIGO DA MARCA NO OPEM BANKING         *
      *                      | 01 - BRADESCO (SERA SEMPRE ESSE)        *
      *                      | 02 - BFF                                *
      *                      | 03 - AGORA                              *
      *                      | 04 - BRADESCO CARD                      *
      *                      | 05 - LOSANGO                            *
      *                      | 06 - NEXT                               *
      * OPBKS-TPO-PSSOA      | TIPO DE DOCUMENTO F = CPF OU J CNPJ     *
      * OPBKS-CNPJ           | FORMATACAO DO CNPJ QUANDO TIPO = J      *
      *   OPBKS-NUM-CNPJ     | NUMERO PRINCIPAL DO CNPJ                *
      *   OPBKS-FIL-CNPJ     | NUMERO DA FILIAL DO CNPJ                *
      *   OPBKS-CTL-CNPJ     | NUMERO DE CONTROLE DO CNPJ              *
      * OPBKS-CPF            | FORMATACAO DO CPF QUANDO TIPO = F       *
      *   OPBKS-RESERVA      | RESERVA ZERADA                          *
      *   OPBKS-NUM-CPF      | NUMERO PRINCIPAL DO CPF                 *
      *   OPBKS-CTL-CPF      | NUMERO DE CONTROLE DO CPF               *
      * OPBKS-BRANCHCODE     | CODIGO DA AGENCIA                       *
      * OPBKS-NUMBER         | NUMERO DA CONTA                         *
      * OPBKS-CREDITMODALITY | MODALIDADE CREDITO CONFORME O DCOM3040  *
      *                      | PRODUCT TYPE + PRODUTCT SUB TYPE        *
      * OPBKS-STARTDATE      | DATA INICIO DE PESQUISA DOS CONTRATOS   *
      * OPBKS-ENDDATE        | DATA FINAL DE PESQUISA DOS CONTRATOS    *
      * OPBKS-CONSENTID      | CODIGO DO CONSETIMENTO DO OPEM BANKING  *
      * OPBKS-STATUS         | CODIGO IDENTIFICADOR DO PROCESSAMENTO   *
      *                      | 0 - SUCESSO SEM NOVO CONTRATO *         *
      *                      | 1 - SUCESSO COM NOVO CONTRATO           *
      *                      | 2 - FALHA NA BUSCA                      *
      * OPBKS-SOURCESYSTEM   | CODIGO DO SISTEMA DE ORIGEM (EX. DCOM)  *
      * OPBKS-CONTRACTNUMNER | NUMERO DO CONTRATO                      *
      * OPBKS-PORTIFOLIO     | CODIGO DA CARTEIRA                      *
      * OPBKS-IPOCCODE       | CODIGO IDENTIFICADOR OPERACAO FINACEIRA *
      ******************************************************************
       01  OPBKS-REGISTRO.
           05 OPBKS-BRANDCODE         PIC  9(002).
           05 OPBKS-PONTO-VIRG-01     PIC  X(001).
           05 OPBKS-TPO-PSSOA         PIC  X(001).
           05 OPBKS-PONTO-VIRG-02     PIC  X(001).
           05 OPBKS-CNPJ.
              10 OPBKS-NUM-CNPJ       PIC  9(008).
              10 OPBKS-FIL-CNPJ       PIC  9(004).
              10 OPBKS-CTL-CNPJ       PIC  9(002).
           05 OPBKS-CPF   REDEFINES   OPBKS-CNPJ.
              10 FILLER               PIC  9(003).
              10 OPBKS-NUM-CPF        PIC  9(009).
              10 OPBKS-CTL-CPF        PIC  9(002).
           05 OPBKS-PONTO-VIRG-03     PIC  X(001).
           05 OPBKS-BRANCHCODE        PIC  9(005).
           05 OPBKS-PONTO-VIRG-04     PIC  X(001).
           05 OPBKS-NUMBER            PIC  9(013).
           05 OPBKS-PONTO-VIRG-05     PIC  X(001).
           05 OPBKS-CREDITMODALITY    PIC  9(004).
           05 OPBKS-PONTO-VIRG-06     PIC  X(001).
           05 OPBKS-STARTDATE         PIC  X(010).
           05 OPBKS-PONTO-VIRG-07     PIC  X(001).
           05 OPBKS-ENDDATE           PIC  X(010).
           05 OPBKS-PONTO-VIRG-08     PIC  X(001).
           05 OPBKS-CONSENTID         PIC  X(100).
           05 OPBKS-PONTO-VIRG-09     PIC  X(001).
           05 OPBKS-STATUS            PIC  X(001).
           05 OPBKS-PONTO-VIRG-10     PIC  X(001).
           05 OPBKS-SOURCESYSTEM      PIC  X(004).
           05 OPBKS-PONTO-VIRG-11     PIC  X(001).
           05 OPBKS-CONTRACTNUMNER    PIC  9(017).
           05 OPBKS-PONTO-VIRG-12     PIC  X(001).
           05 OPBKS-PORTIFOLIO        PIC  X(003).
           05 OPBKS-PONTO-VIRG-13     PIC  X(001).
           05 OPBKS-IPOCCODE          PIC  X(067).
