      ******************************************************************
      * SISTEMA : DCOM - DESCONTO COMERCIAL                            *
      * ARQUIVO : LAYOUT DAS SOLICITACOES DAS NOVAS OPERACOES DO OPEM  *
      *           BANKING                                              *
      * AUTOR   : MILTON NOGUEIRA                                      *
      * DDS     : PAULO ROBERTO                                        *
      * EMPRESA : BRQ SOLUTION'S                                       *
      * DATA    : SET/2021                                             *
      * TAMANHO : 0200 BYTES                                           *
      ******************************************************************
      * CAMPO                | DESCRICAO                               *
      *----------------------+-----------------------------------------*
      * OPBKE-BRANDCODE      | CODIGO DA MARCA NO OPEM BANKING         *
      *                      | 01 - BRADESCO *                         *
      *                      | 02 - BFF                                *
      *                      | 03 - AGORA                              *
      *                      | 04 - BRADESCO CARD                      *
      *                      | 05 - LOSANGO                            *
      *                      | 06 - NEXT                               *
      * OPBKE-TPO-PSSOA      | TIPO DE DOCUMENTO F = CPF OU J CNPJ     *
      * OPBKE-CNPJ           | FORMATACAO DO CNPJ QUANDO TIPO = J      *
      *   OPBKE-NUM-CNPJ     | NUMERO PRINCIPAL DO CNPJ                *
      *   OPBKE-FIL-CNPJ     | NUMERO DA FILIAL DO CNPJ                *
      *   OPBKE-CTL-CNPJ     | NUMERO DE CONTROLE DO CNPJ              *
      * OPBKE-CPF            | FORMATACAO DO CPF QUANDO TIPO = F       *
      *   OPBKE-RESERVA      | RESERVA ZERADA                          *
      *   OPBKE-NUM-CPF      | NUMERO PRINCIPAL DO CPF                 *
      *   OPBKE-CTL-CPF      | NUMERO DE CONTROLE DO CPF               *
      * OPBKE-BRANCHCODE     | CODIGO DA AGENCIA                       *
      * OPBKE-NUMBER         | NUMERO DA CONTA                         *
      * OPBKE-CREDITMODALITY | MODALIDADE CREDITO CONFORME O DCOM3040  *
      *                      | PRODUCT TYPE + PRODUTCT SUB TYPE        *
      * OPBKE-STARTDATE      | DATA INICIO DE PESQUISA DOS CONTRATOS   *
      * OPBKE-ENDDATE        | DATA FINAL DE PESQUISA DOS CONTRATOS    *
      * OPBKE-CONSENTID      | CODIGO DO CONSETIMENTO DO OPEM BANKING  *
      ******************************************************************
       01  OPBKE-REGISTRO.
           05 OPBKE-BRANDCODE         PIC  9(002).
           05 OPBKE-TPO-PSSOA         PIC  X(001).
           05 OPBKE-CNPJ.
              10 OPBKE-NUM-CNPJ       PIC  9(008).
              10 OPBKE-FIL-CNPJ       PIC  9(004).
              10 OPBKE-CTL-CNPJ       PIC  9(002).
           05 OPBKE-CPF   REDEFINES   OPBKE-CNPJ.
              10 FILLER               PIC  9(003).
              10 OPBKE-NUM-CPF        PIC  9(009).
              10 OPBKE-CTL-CPF        PIC  9(002).
           05 OPBKE-BRANCHCODE        PIC  9(005).
           05 OPBKE-NUMBER            PIC  9(013).
           05 OPBKE-CREDITMODALITY    PIC  9(004).
           05 OPBKE-STARTDATE         PIC  X(010).
           05 OPBKE-ENDDATE           PIC  X(010).
           05 OPBKE-CONSENTID         PIC  X(100).
           05 FILLER                  PIC  X(041).
