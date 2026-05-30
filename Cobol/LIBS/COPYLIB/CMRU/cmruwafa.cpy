      ******************************************************************
      * NOME BOOK : CMRUWAFA                                           *
      * DESCRICAO : INTERFACE DO SERVICO FUNCIONAL BATCH CMRU2AFT.     *
      *             DE/PARA DE NUMERO DE CONTRATO                      *
      * DATA      : ABR/2013                                           *
      * AUTOR     : KLEBER LOURENCO DIAS                               *
      * EMPRESA   : BSI TECNOLOGIA                                     *
      * COMPONENTE: CMRU - CONVIVENCIA E MIGRACAO - CREDITO RURAL      *
      * TAMANHO   : CMRUWAFA-TAM-LAYOUT                                *
      ******************************************************************
      * DATA       AUTOR       DESCRICAO / MANUTENCAO                  *
      ******************************************************************
      * XXXXXXXXXX XXXXXXXXX   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX         *
      ******************************************************************
      * BLOCO DE IDENTIFICACAO                                         *
      * CMRUWAFA-COD-LAYOUT         = CODIGO DESTE LAYOUT              *
      * CMRUWAFA-TAM-LAYOUT         = TAMANHO DO REGISTRO              *
      *                                                                *
      * BLOCO DE ENTRADA                                               *
      * CMRUWAFA-E-CPSSOA-JURID     = CODIGO PESSOA JURIDICA           *
      * CMRUWAFA-E-CTPO-CONTR       = CODIGO TIPO CONTRATO             *
      * CMRUWAFA-E-NSEQ-CONTR       = NUMERO SEQUENCIAL CONTRATO       *
      * CMRUWAFA-E-CCONTR-LGADO     = NUMERO CONTRATO LEGADO           *
      *                                                                *
      * BLOCO DE SAIDA                                                 *
      * CMRUWAFA-S-CPSSOA-JURID     = CODIGO PESSOA JURIDICA           *
      * CMRUWAFA-S-CTPO-CONTR       = CODIGO TIPO CONTRATO             *
      * CMRUWAFA-S-NSEQ-CONTR       = NUMERO SEQUENCIAL CONTRATO       *
      * CMRUWAFA-S-CCONTR-LGADO     = NUMERO CONTRATO LEGADO           *
      *                                                                *
      ******************************************************************
           05 CMRUWAFA-HEADER.
              10 CMRUWAFA-COD-LAYOUT      PIC X(008) VALUE 'CMRUWAFA'.
              10 CMRUWAFA-TAM-LAYOUT      PIC 9(005) VALUE      00091.
           05 CMRUWAFA-REGISTRO.
              10 CMRUWAFA-BLOCO-RETORNO.
                 15 CMRUWAFA-COD-RETORNO           PIC  9(002).
                 15 CMRUWAFA-COD-ERRO              PIC  X(004).
                 15 CMRUWAFA-COD-MENSAGEM          PIC  X(008).
              10 CMRUWAFA-BLOCO-ENTRADA.
                 15 CMRUWAFA-E-CPSSOA-JURID        PIC  9(010).
                 15 CMRUWAFA-E-CTPO-CONTR          PIC  9(003).
                 15 CMRUWAFA-E-NSEQ-CONTR          PIC  9(010).
                 15 CMRUWAFA-E-CCONTR-LGADO        PIC  9(009).
              10 CMRUWAFA-BLOCO-SAIDA.
                 15 CMRUWAFA-S-CPSSOA-JURID        PIC  9(010).
                 15 CMRUWAFA-S-CTPO-CONTR          PIC  9(003).
                 15 CMRUWAFA-S-NSEQ-CONTR          PIC  9(010).
                 15 CMRUWAFA-S-CCONTR-LGADO        PIC  9(009).