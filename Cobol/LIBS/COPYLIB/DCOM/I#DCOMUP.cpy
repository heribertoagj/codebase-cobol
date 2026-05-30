      ******************************************************************
      * SISTEMA : DCOM -                                     I#DCOMUP  *
      * TAMANHO : 0200 BYTES                                           *
      * ARQUIVO : CONSULTA DE PRODUTOS VINCULADOS A CONTA              *
      * INCLUDE DA TABELA DCOMB0P3 - DB2PRD.TSOLTC_ENCRR_CTA_DESC      *
      ******************************************************************
001    01 UP-REGISTRO.
001       05 UP-CHAVE.
001 03       10 UP-CBCO                PIC  9(003).
003 05       10 UP-CAG-BCRIA           PIC  9(005).
008 12       10 UP-CCTA-CRIA-CLI       PIC  9(012).
020 17    05 UP-NSOLTC-ENCRR           PIC  9(017).
037 10    05 UP-CPSSOA-JURID           PIC  9(010).
047 03    05 UP-CTPO-NEGOC             PIC  9(003).
050 10    05 UP-NSEQ-NEGOC             PIC  9(010).
060 08    05 UP-DSOLTC-ENCRR-CTA       PIC  X(008).
060 08    05 FILLER REDEFINES  UP-DSOLTC-ENCRR-CTA.
060 02       10 UP-DD-ENCRR-CTA        PIC  9(002).
062 02       10 UP-MM-ENCRR-CTA        PIC  9(002).
064 04       10 UP-AAAA-ENCRR-CTA      PIC  9(004).
068 08    05 UP-DPREVT-EFTVC-DSVNC     PIC  X(008).
068 08    05 FILLER REDEFINES  UP-DPREVT-EFTVC-DSVNC.
068 02       10 UP-DD-EFTVC-DSVNC      PIC  9(002).
070 02       10 UP-MM-EFTVC-DSVNC      PIC  9(002).
072 04       10 UP-AAAA-EFTVC-DSVNC    PIC  9(004).
076 15    05 UP-CPF-CNPJ               PIC  9(015).
076 15    05 FILLER REDEFINES  UP-CPF-CNPJ.
076 09       10 UP-CPF-CNPJ-PRINC      PIC  9(009).
085 04       10 UP-CPF-CNPJ-FILIAL     PIC  9(004).
089 02       10 UP-CPF-CNPJ-CTRL       PIC  9(002).
091 04    05 UP-COD-SIST               PIC  X(004).
095 17    05 UP-CHAVE-PRODT            PIC  X(017).
095 17    05 FILLER REDEFINES  UP-CHAVE-PRODT.
095 04       10 UP-CENTRO-CUSTO        PIC  X(004).
099 04       10 UP-DANO-OPER-DESC      PIC  X(004).
103 09       10 UP-NSEQ-OPER-DESC      PIC  X(009).
112 01    05 UP-SIT-DESVN              PIC  X(001).
113 87    05 FILLER                    PIC  X(087).
