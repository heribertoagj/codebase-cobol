      *---------------------------------------------------------------*
      *   NOME INC             -DDNAME-                LENGTH  0248   *
      *   I#DCOMRS              ------                 SISTEMA DCOM   *
      *---------------------------------------------------------------*
      *                                                               *
      *   DESCRICAO: LAYOUT DA RESP E SEGMENTACAO RESC.               *
      *                                                               *
      *   PROPOSTA DDS - 09/0208.                                     *
      *---------------------------------------------------------------*

       01  DCOMRS-REG.
           05 RS-CAG-BCRIA                   PIC 9(05).
           05 RS-CCTA-BCRIA-CLI              PIC 9(07).
           05 RS-NOME                        PIC X(35).
           05 RS-CARTEIRA-ORIGEM             PIC X(03).
           05 FILLER                         PIC X(01).
           05 RS-COD-FIRMA                   PIC X(03).
           05 RS-COD-CARTEIRA-MORA           PIC X(03).
           05 RS-VALOR-RESPONSA              PIC 9(13)  COMP-3.
           05 RS-NUMERO-CONTRATO             PIC 9(07).
           05 RS-DT-VENCI                    PIC 9(08).
           05 RS-NUM-PRESTACAO               PIC 9(02).
           05 RS-VALOR-PARC-PRESTACAO        PIC 9(11)  COMP-3.
           05 RS-SALDO                       PIC 9(01).
           05 RS-GARANTIA                    PIC 9(01).
           05 RS-CPF-CNPJ                    PIC 9(09).
           05 RS-FILIAL-CPF-CNPJ             PIC 9(05).
           05 RS-CONTROLE-CPF-CNPJ           PIC 9(02).
           05 RS-NP-DANO-BASE                PIC 9(04).
           05 RS-NP-CDEPDC                   PIC 9(05).
           05 RS-NP-CSEQ-STUDO               PIC 9(09).
           05 RS-COD-DEPENDENCIA             PIC 9(05).
           05 RS-SEQUENCIA                   PIC X(02).
           05 RS-COD-GARANTIA                PIC 9(03).
           05 RS-PERCEN-GARANT               PIC 9(03).
           05 RS-DATA-INICIO                 PIC 9(08).
           05 RS-PRAZO-OPER                  PIC 9(05).
           05 RS-TX-OPER                     PIC 9(08)V9(05).
           05 RS-SIT-CONTABIL                PIC X(02).
           05 RS-SALDO-PRINCI                PIC 9(15)V99.
           05 RS-ENCARGOS                    PIC 9(15)V99.
           05 RS-CENTRO-CUSTO                PIC X(04).
           05 RS-SALDO-VENC                  PIC 9(15)V99.
           05 RS-SALDO-UTILI                 PIC 9(15)V99.
           05 RS-DIG-CONTRATO                PIC X(01).
           05 FILLER                         PIC X(17).
