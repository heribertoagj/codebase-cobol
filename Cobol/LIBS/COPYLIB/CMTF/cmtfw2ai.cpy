      ******************************************************************
      * NOME BOOK : CMTFW2AI                                           *
      * DESCRICAO : INCLUIR REGISTRO NA TABELA CMTFB016                *
      * DATA      : 09/2016                                            *
      * TAMANHO   : 01334 BYTES                                        *
      ******************************************************************
      * DMOVTO-TARIF        = DATA MOVIMENTO                           *
      * CTPO-GSTAO-TARIF    = TIPO GESTAO                              *
      * CJUNC-DEPDC         = CODIGO DA JUNCAO                         *
      * CCTA-CLI            = NUM CONTA                                *
      * CSEQ-TARIF-CLI      = SEQ TARIFA                               *
      * CPRODT-OPER-DEFLT   = CODIGO PRODUTO                           *
      * COPER-PRODT-SERVC   = CODIGO OPERACAO                          *
      * CCONDC-ECONC        = CODIGO CONDICAO ECONOMICA                *
      * CPSSOA-JURID-CONTR  = CODIGO PESSOA JURIDICA                   *
      * CTPO-CONTR-NEGOC    = CODIGO TIPO CONTRATO NEGOCIO             *
      * NSEQ-CONTR-NEGOC    = NUMERO SEQUENCIAL CONTRATO NEGOCIO       *
      * CSIT-OPER-REALZ     = CODIGO SITUACAO OPERACAO REALIZADA       *
      * CFUNCL-GSTAO-TARIF  = CODIGO FUNCIONALIDADE GESTAO TARIFA      *
      * CMOTVO-INCON-MOVTO  = CODIGO MOTIVO INCONSISTENCIA MOVIMENTO   *
      * CSIST               = CODIGO SISTEMA                           *
      * CPROG               = CODIGO PROGRAMA                          *
      * HMANUT-REG          = HORA MANUTENCAO REGISTRO                 *
      * RJUSTF-MOTVO-RPROC  = DESC. JUSTIF. MOTIVO REPROCESSAMENTO     *
      * RJUSTF-DEFER-RPROC  = DESC. JUSTIF. DEFERIMENTO REPROCESSAMENTO*
      ******************************************************************
       05 CMTFW2AI-HEADER.
          10 CMTFW2AI-COD-LAYOUT            PIC X(008) VALUE 'CMTFW2AI'.
          10 CMTFW2AI-TAM-LAYOUT            PIC 9(005) VALUE 01334.
      *
       05 CMTFW2AI-REGISTRO.
          10 CMTFW2AI-DMOVTO-TARIF          PIC X(010).
          10 CMTFW2AI-CTPO-GSTAO-TARIF      PIC 9(004).
          10 CMTFW2AI-CJUNC-DEPDC           PIC 9(005).
          10 CMTFW2AI-CCTA-CLI              PIC 9(007).
          10 CMTFW2AI-CSEQ-TARIF-CLI        PIC 9(007).
          10 CMTFW2AI-CPRODT-OPER-DEFLT     PIC 9(008).
          10 CMTFW2AI-COPER-PRODT-SERVC     PIC 9(005).
          10 CMTFW2AI-CCONDC-ECONC          PIC 9(005).
          10 CMTFW2AI-CPSSOA-JURID-CONTR    PIC 9(010).
          10 CMTFW2AI-CTPO-CONTR-NEGOC      PIC 9(003).
          10 CMTFW2AI-NSEQ-CONTR-NEGOC      PIC 9(010).
          10 CMTFW2AI-CSIT-OPER-REALZ       PIC 9(003).
          10 CMTFW2AI-CFUNCL-GSTAO-TARIF    PIC 9(001).
          10 CMTFW2AI-CMOTVO-INCON-MOVTO    PIC 9(005).
          10 CMTFW2AI-CSIST                 PIC X(004).
          10 CMTFW2AI-CPROG                 PIC X(008).
          10 CMTFW2AI-HMANUT-REG            PIC X(026).
          10 CMTFW2AI-RJUSTF-MOTVO-RPROC    PIC X(600).
          10 CMTFW2AI-RJUSTF-DEFER-RPROC    PIC X(600).
      *
