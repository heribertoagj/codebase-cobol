      *---------------------------------------------------------------*
      *                   Q I N T E S S                               *
      *---------------------------------------------------------------*
      * INCLUDE.....: I#DCOMXT - PARA MODULO DCOM5181                 *
      * GERACAO.....: NOVEMBRO/2020                                   *
      *---------------------------------------------------------------*
      * OBJETIVO....: AREA DE COMUNICACAO COM A API DCOM5181          *
      *                                                               *
      *---------------------------------------------------------------*
      * PROJETO ....: REFORMULACAO MENSAGERIA BACEN - RECOR           *
      *---------------------------------------------------------------*
      **
       01  DCOMXT-AREA.
           05  DCOMXT-ENVIO.
             10  DCOMXT-PGM-CHAMADOR         PIC  X(008).
             10  DCOMXT-NRO-CTRLIF           PIC  X(020).
      **
      *************** AREA DE DADOS DE PAGINACAO ***************
      **
           05 DCOMXT-PAGINACAO.

             10  DCOMXT-NUM-PAG-ATUAL-E      PIC  9(003).
             10  DCOMXT-NUM-PAG-MAX-E        PIC  9(003).

             10  DCOMXT-COD-REF-PAG-I        PIC  9(005).
             10  DCOMXT-NUM-REF-BCCOR-I      PIC  X(011).

             10  DCOMXT-COD-REF-PAG-F        PIC  9(005).
             10  DCOMXT-NUM-REF-BCCOR-F      PIC  X(011).

             10  DCOMXT-IND-PAGINA           PIC  X(001) VALUE  SPACES.
                 88 DCOMXT-P-INICIAL         VALUE 'I'.
                 88 DCOMXT-P-PRIMEIRA        VALUE 'P'.
                 88 DCOMXT-P-SEGUINTE        VALUE 'S'.
                 88 DCOMXT-P-ANTERIOR        VALUE 'A'.
                 88 DCOMXT-P-ULTIMA          VALUE 'U'.


      **
      *************** AREA DE DADOS DE RETORNO ***************
      **
           05  DCOMXT-RETORNO.
             10  DCOMXT-COD-RET              PIC  9(005).
      **
      **=======  00 - OK
      **=======  01 - DADOS INCONSISTENTES
      **=======  03 - NAO ENCONTRADO
      **=======  99 - ERRO DB2
             10  DCOMXT-MENSAGEM             PIC  X(079).
             10  FILLER                      PIC  X(050).
             10  DCOMXT-SQLCA                PIC  X(136).
             10  DCOMXT-ERRO-AREA            PIC  X(107).
             10  DCOMXT-DTHR-BACEN           PIC  X(019).
             10  DCOMXT-DTMOVTO              PIC  X(010).
             10  DCOMXT-NUM-PAG-ATUAL        PIC  9(003).
             10  DCOMXT-NUM-PAG-MAX          PIC  9(003).
             10  DCOMXT-DT-INI-PESQ          PIC  X(007).
             10  DCOMXT-DT-FIM-PESQ          PIC  X(007).
             10  DCOMXT-NRO-CTRL-IF-S        PIC  X(020).
             10  DCOMXT-QT-CEDULAS           PIC  X(009).
             10  DCOMXT-NUM-CONSULTAS        PIC  9(005).
             10  DCOMXT-REG-OCORRENCIA       OCCURS 3 TIMES.
                 15  DCOMXT-COD-REF-PAG               PIC 9(005).
                 15  DCOMXT-OPERACAO                  PIC 9(014).
                 15  DCOMXT-NUM-REF-BCCOR             PIC X(011).
                 15  DCOMXT-NUM-ORDEM-DESTC           PIC 9(003).
                 15  DCOMXT-ANO-MES-REF               PIC X(007).
                 15  DCOMXT-SIT-OPCOR                 PIC X(034).
                 15  DCOMXT-MOT-RECSA-DAD-DINMCO      PIC X(070).
                 15  DCOMXT-DT-HR-IMPTC-SCR           PIC X(019).
