$PBExportHeader$nvo_controles_datawin.sru
$PBExportComments$Objeto donde estan las funciones para los dw (barra, condicion, etc.)
forward
global type nvo_controles_datawin from nonvisualobject
end type
end forward

global type nvo_controles_datawin from nonvisualobject
event type integer ue_validadocumentoelectronico ( string as_clave,  string as_numerodocumento )
end type
global nvo_controles_datawin nvo_controles_datawin

type variables
Public:
	Long    ii_sesion_id
	Integer ii_n_menu = 0
	
	String  is_modulo
	Boolean ib_mensaje_error = False
	Integer ii_coneccion // Que base esta trabajando
	
/* Globales del sistema */
	Long    il_usuario
	Long    il_empresa
	Long    il_sucursal
	Long    il_moneda
	Long    il_bodega						// 28/04/2017
	Integer ii_modulo
	Integer ii_fila
	Date    ida_fecha_sistema
	Long    il_nulo
	String  is_nulo
	Date    ida_fechaDefault	    // Fecha default del PB
	String  is_nombreArchivoIni	 // 30-01-2020


	// Nro de digitos de los comprobantes contables
	Long    il_nroDigitosCompContables = 8
	String  is_nroDigitosCompContables

	Long    il_ManejadorMail			// 20-10-2017
	
	
/* Contabilidad */		
	Long    il_ejercicio, il_periodo,il_moneda_base,il_moneda_ext
	String  is_lotes
	
/* Inventario */
	Integer ii_cant, ii_reconoce
	String  as_nombre, is_codigo_prod
//	String is_tip_trans_r1,is_tip_trans_r2,is_tip_trans_r3,is_prod_like,is_sucursal_f
//	date idt_desde_r,idt_hasta_r


/* Documentos electronicos */
	// 24-11-2014
	Constant String cte_soloNumeros = '[0-9]'
	Constant Long   cte_repiteNumeros = 5
	
	String  is_formatoValido
	String  is_formatoMsg			// est-pem-secuencia [###-###-#########] segun prms
	Long    il_nroDigitoEsta		// Definido en archivo ini del sistema Formato
	Long    il_nroDigitoPEmi		// Definido en archivo ini del sistema Formato
	Long    il_nroDigitoSecu		// Definido en archivo ini del sistema Formato
	// FIN 24-11-2014

end variables

forward prototypes
public subroutine of_run_even_wind (graphicobject ago_padre, string as_name_event, integer ai_prm_uno, integer ai_prm_dos)
public function string of_name_month (integer ai_num_mes)
public function string of_name_day (integer ai_num_day)
public function string of_fecha_letras (date ada_fecha, string as_sn_dia)
public subroutine of_clean_query (datawindow adw_actual)
public subroutine of_control_barra (datawindow adw_actual, m_menu_main am_actual, integer ai_boton1)
public subroutine of_sesion_id ()
public function boolean of_activa_opcion (datawindow adw_control, string as_opcion)
public function boolean of_const_condicion (datawindow adw_actual, ref string as_condiciones, boolean ab_buscar)
public subroutine of_cabecera_sucursal (datawindow adw_fuente, integer ai_cabecera)
public subroutine of_ayuda ()
public function boolean of_verifica_datos (long al_fila, datawindow adw_fuente)
public function boolean of_const_condicion (datawindow adw_actual, ref string as_condiciones, boolean ab_buscar, boolean ab_estadoactualizar)
public subroutine of_cabecera_sucursal (datastore ads_fuente, integer ai_cabecera)
public function long of_showerrorbdd (transaction at_transaccionbdd, string as_mensaje)
public function string of_getnumerofactura (long al_codigoempresa, string as_numeropacking)
public subroutine of_formatodocumentoselectronicos ()
public subroutine of_msgerror (string as_error)
public function string of_getsofiaini (string as_grupo, string as_clave, string as_default)
public function integer of_validanumeroautorizacion (string as_numeroautorizacion)
public function string of_validanombre (string as_nombregeneral, integer ai_numerocaracteres)
public function integer of_validarclaveacceso (string as_numeroautorizacion)
public function string of_gettitulomodulo ()
public function integer msg_err (string as_msg)
public subroutine of_inserta_blanco_dddw (datawindow adw_control)
public function string of_getempresaini (string as_grupo, string as_clave, string as_default)
public subroutine of_poswinfinal (window aw_actual)
public subroutine of_poswininicio (window aw_actual)
public function integer of_exportarconlogo (datawindow adw_datos, string as_nombrearchivo, string as_carpetadestino)
public function integer of_exportarconlogo (datastore ads_datos, string as_nombrearchivo, string as_carpetadestino)
end prototypes

event type integer ue_validadocumentoelectronico(string as_clave, string as_numerodocumento);/*
Name: ue_validaDocumentoElectronico
	Valida que el establecimiento y punto de emision sean de la empresa

Create: 24-11-2014

PRMS
	as_clave
	as_numeroDocumento
*/
String ls_valor, ls_aux, ls_estPem[]
Long ll_pos, ll_posIni = 1, ll_secu = 1, ll_cont
Boolean lb_valido = False


// ls_valor = This.of_getSofiaIni('Documentos Electronicos', as_clave, '001-001')
ls_valor = This.of_getEmpresaIni('Documentos Electronicos', as_clave, '001-001')	// 16-08-2018

ls_aux = ls_valor
ll_pos = Pos(ls_aux, ',', ll_posIni)
DO WHILE ll_pos > 0
	ls_estPem[ll_secu] = Left(ls_aux, ll_pos - 1)
	ls_aux = Mid(ls_aux, ll_pos + 1, Len(ls_aux) - ll_pos)
	ll_secu ++
	ll_pos = Pos(ls_aux, ',', ll_posIni)
LOOP
ls_estPem[ll_secu] = ls_aux

FOR ll_cont = 1 TO ll_secu
	ll_pos = Pos(as_numeroDocumento, ls_estPem[ll_cont] + '-', ll_posIni)
	IF ll_pos > 0 THEN
		lb_valido = True
		Exit
	END IF
NEXT

IF NOT lb_valido THEN
	MessageBox('Error', 'Documento no pertenece a la empresa.' + '~r~n' + &
		'Revise código de establecimiento o punto de emisión')
	Return -1
END IF

Return 1

end event

public subroutine of_run_even_wind (graphicobject ago_padre, string as_name_event, integer ai_prm_uno, integer ai_prm_dos);// Funcion para ejecutar eventos del padre del DW actual
IF ago_padre.TypeOf() = window! THEN
	ago_padre.TriggerEvent(as_name_event,ai_prm_uno,ai_prm_dos)
ELSE // tab! w_inicio: pantalla principal MDI
	w_inicio.GetActiveSheet().TriggerEvent(as_name_event,ai_prm_uno,ai_prm_dos)
END IF

end subroutine

public function string of_name_month (integer ai_num_mes);/*
Nombre: of_name_month
Parametros
	ai_num_mes: Número del mes
Retorno
	String: Nombre del mes
*/
String ls_name_mes[], ls_mes

IF NOT (ai_num_mes >= 1 AND ai_num_mes <= 12) THEN 
	SetNull(ls_mes)
	Return ls_mes
END IF
ls_name_mes[] = {'Enero','Febrero','Marzo','Abril','Mayo','Junio','Julio', &
		'Agosto','Septiembre','Octubre','Noviembre','Diciembre'}

Return ls_name_mes[ai_num_mes]
end function

public function string of_name_day (integer ai_num_day);/*
Nombre: of_name_day
Parametros
	ai_num_day: Número del dia
Retorno
	String: Nombre del dia
*/
String ls_name_day[], ls_day

IF NOT (ai_num_day >= 1 AND ai_num_day <= 7) THEN 
	SetNull(ls_day)
	Return ls_day
END IF
ls_name_day[] = {'Lunes','Martes','Miércoles','Jueves','Viernes','Sábado','Domingo'}

Return ls_name_day[ai_num_day]
end function

public function string of_fecha_letras (date ada_fecha, string as_sn_dia);/*
Nombre: of_fecha_letras
Parametros
	ada_fecha: fecha a transformarse
	as_sn_dia: Si se coloca el dia [s,n]
Retorno
	String: La fecha en letras [Lunes, 1 de Enero de(l) 2000]
*/
Integer li_anio, li_mes, li_dia, li_ndia
String ls_fecha_letras = '', ls_name_day, ls_name_mes, ls_union = ' del '

li_anio = Year(ada_fecha)
li_mes = Month(ada_fecha)
li_dia = Day(ada_fecha)

as_sn_dia = Upper(as_sn_dia)
IF Trim(as_sn_dia) = 'S' THEN
	li_ndia = DayNumber(ada_fecha) - 1
	IF li_ndia = 0 THEN li_ndia = 7
	ls_name_day = of_name_day(li_ndia)
	ls_fecha_letras = Trim(ls_name_day) + ', '
END IF
ls_fecha_letras += String(li_dia,'00') + ' de'
ls_name_mes = of_name_month(li_mes)

IF li_anio < 2000 THEN ls_union = ' de '
ls_fecha_letras += ' ' + Trim(ls_name_mes) + ls_union + String(li_anio,'0000')

Return ls_fecha_letras
end function

public subroutine of_clean_query (datawindow adw_actual);//	Funcion para limpiar los valores 
//	iniciales del DataWindow
Long li_contador, li_total_columnas

// Numero de columnas del DW
li_total_columnas	= Integer(adw_actual.Object.DataWindow.Column.Count)

// Recorre DW para ver condiciones
FOR li_contador = 1 TO li_total_columnas
	// Validacion
	adw_actual.Modify("#"+String(li_contador) + ".Validation = ''")
	// Inicializa en null
	adw_actual.Modify("#"+String(li_contador) + ".Initial = 'empty'")
	CHOOSE CASE adw_actual.Describe("#"+String(li_contador) + ".Edit.Style")
		CASE 'edit'
			adw_actual.Modify("#"+String(li_contador) + ".Edit.DisplayOnly = no")
			adw_actual.Modify("#"+String(li_contador) + ".Edit.Required = no")
		CASE 'dddw'
			adw_actual.Modify("#"+String(li_contador) + ".dddw.Required = no")
		CASE 'ddlb'
			adw_actual.Modify("#"+String(li_contador) + ".DDLB.Required=No")
		CASE 'editmask'
			adw_actual.Modify("#"+String(li_contador) + ".EditMask.Required=No")
	END CHOOSE
NEXT

end subroutine

public subroutine of_control_barra (datawindow adw_actual, m_menu_main am_actual, integer ai_boton1);/*
Nombre: of_control_barra
	Realiza los cambios en la barra de herramientas, activando y 
	desactivando botones.

Parametros
	adw_actual: Datawindow actual 
	am_actual: Menu actual
	ai_boton1: Indica que boton fue presionado

Retorno
	Nada
*/

// Definicion de variables
Boolean lb_eof = False, lb_bof = False, &
	lb_new = False, lb_del = False, lb_save = False, &
	lb_dete = False, lb_cerr = False, lb_actu = False, &
	lb_busc = False, lb_recu = False, lb_list = False
Boolean lb_botonImprimir = False, lb_botonesCopiarExportar = False
Integer li_registro_actual
Long ll_registro_total
String ls_dw, ls_menu, ls_insert, ls_update, ls_delete
String ls_botonImprimir, ls_botonesCopiarExportar


// Proceso
li_registro_actual = adw_actual.GetRow()
ll_registro_total = adw_actual.RowCount()

IF ll_registro_total = 0 OR ll_registro_total = 1 THEN
	lb_bof = True
	lb_eof = True
ELSEIF li_registro_actual <= 1 THEN
	lb_bof = True
	lb_eof = False
ELSEIF li_registro_actual = ll_registro_total THEN
	lb_bof = False
	lb_eof = True
END IF


CHOOSE CASE ai_boton1
	CASE 1	// nuevo
		lb_del  = True
		lb_save = True
		lb_actu = True
		lb_bof = True
		lb_eof = True
	CASE 2	// buscar
		lb_cerr = True
		lb_recu = True
		lb_bof = True
		lb_eof = True
	CASE 3	// aceptar
		lb_dete = True
		lb_bof = True
		lb_eof = True
	CASE ELSE
		IF ll_registro_total > 1 THEN lb_list = True
		IF ll_registro_total > 0 THEN
			lb_new = True
			lb_del  = True
			lb_save = True
			lb_cerr = True
			lb_actu = True
			lb_busc = True
		ELSE
			lb_new = True
			lb_cerr = True
			lb_busc = True
		END IF
END CHOOSE

// Botones de movimiento
am_actual.m_edicion.m_primero.Enabled   = NOT lb_bof
am_actual.m_edicion.m_anterior.Enabled  = NOT lb_bof
am_actual.m_edicion.m_siguiente.Enabled = NOT lb_eof
am_actual.m_edicion.m_ultimo.Enabled    = NOT lb_eof

// Barra de herramientas
am_actual.m_edicion.m_nuevo.Enabled    			= lb_new
am_actual.m_edicion.m_eliminar.Enabled 			= lb_del
am_actual.m_edicion.m_grabar.Enabled  				= lb_save
am_actual.m_edicion.m_detener.Enabled  			= lb_dete
am_actual.m_edicion.m_cerrar.Enabled				= lb_cerr
am_actual.m_edicion.m_actualizarlistas.Enabled	= lb_actu
am_actual.m_edicion.m_buscar.Enabled				= lb_busc
am_actual.m_edicion.m_recuperar.Enabled			= lb_recu
am_actual.m_edicion.m_listados.Enabled				= lb_list

/*
Permisos de actualizacion
*/
ls_dw = adw_actual.ClassName()
ls_menu = adw_actual.Dynamic uf_menu()

// busco permisos
SELECT acce_ban_inse, acce_ban_upda, acce_ban_dele,
	acce_ban_prin, acce_ban_expo
INTO :ls_insert, :ls_update, :ls_delete,
	:ls_botonImprimir, :ls_botonesCopiarExportar
FROM saeacce, saeopci, saeopdw  
WHERE opdw_cod_opci = opci_cod_opci
AND opdw_cod_opci = acce_cod_opci
AND opdw_ctr_dawi = acce_ctr_dawi
AND opci_cod_modu = :ii_modulo
AND opci_ctr_opci = :ls_menu
AND opdw_ctr_dawi = :ls_dw
AND acce_cod_empr = :il_empresa
AND acce_cod_usua = :il_usuario ;

IF Trim(ls_insert) = '0' THEN	lb_new = False
IF Trim(ls_update) = '0' THEN
	IF ll_registro_total > 0 AND ai_boton1 <> 1 THEN lb_save = False
END IF
IF Trim(ls_delete) = '0' THEN
	IF ll_registro_total > 0 AND ai_boton1 <> 1 THEN lb_del = False
END IF

am_actual.m_edicion.m_nuevo.Enabled    = lb_new
//am_actual.m_detalles.m_insertar.Enabled= lb_new

am_actual.m_edicion.m_eliminar.Enabled	= lb_del
//am_actual.m_detalles.m_eliminardetalle.Enabled = lb_del

am_actual.m_edicion.m_grabar.Enabled  	= lb_save

// Botones para reportes
IF ll_registro_total > 0 THEN
	IF ls_botonImprimir = '1' THEN lb_botonImprimir = True
	IF ls_botonesCopiarExportar = '1' THEN lb_botonesCopiarExportar = True
END IF

am_actual.m_archivo.m_imprimir.Enabled = lb_botonImprimir
am_actual.m_edicion.m_copiar.Enabled	= lb_botonesCopiarExportar
am_actual.m_edicion.m_exportar.Enabled = lb_botonesCopiarExportar
// FIN Botones para reportes

end subroutine

public subroutine of_sesion_id ();nvo_valores_iniciales lvalores_iniciales
Long ll_codigoPaquete


/*
GENERAL DEL SISTEMA
*/
// Modulo de Sistema
SELECT modu_des_modu INTO :is_modulo
FROM saemodu
WHERE modu_cod_modu = :ii_modulo;

// Fecha del sistema
ida_fecha_sistema = f_fecha_server()

// Selecciona la sucursal principal
lvalores_iniciales.of_getCodigoSucursalDefecto(il_empresa)	// 13-11-2018

SetNull(il_nulo)
SetNull(is_nulo)

/*
CONTABILIDAD
*/
// Escoge el ejercicio activo
SELECT ejer_cod_ejer INTO :il_ejercicio  
FROM saeejer
WHERE ejer_cod_empr = :il_empresa
AND :ida_fecha_sistema BETWEEN ejer_fec_inil AND ejer_fec_finl;
//AND ejer_est_ejer = 'A';


// Selecciono el periodo		
SELECT prdo_num_prdo INTO :il_periodo  
FROM saeprdo  
WHERE prdo_cod_ejer = :il_ejercicio
AND prdo_cod_empr = :il_empresa
AND prdo_fec_ini <= :ida_fecha_sistema
AND prdo_fec_fin >= :ida_fecha_sistema;

/*
	Selección de la moneda base
*/
SELECT saepcon.pcon_mon_base,
		saepcon.pcon_seg_mone
INTO :il_moneda_base,
	  :il_moneda_ext
FROM saepcon  
WHERE saepcon.pcon_cod_empr = :il_empresa;

if isnull(il_moneda_base) or il_moneda_base = 0 then
	messagebox('Información','No se ha parametrizado la moneda base')
	return
end if


// Documentos electronicos
This.of_formatoDocumentosElectronicos()

// Conprobantes contables
is_nroDigitosCompContables = Fill('0', il_nroDigitosCompContables)

// Inventarios - Bodega por Defecto
IF ii_modulo = 10 OR ii_modulo = 7 THEN lvalores_iniciales.of_getCodigoBodegaDefecto(il_empresa, il_sucursal)	// 13-11-2018

// Finca
lvalores_iniciales.of_getCodigoFincaDefecto(il_empresa)	// 01-09-2017

// Porgrama de correo
lvalores_iniciales.of_getCodigoMailDefecto(il_empresa)	// 20-10-2017

// Nombre del archivo INI
is_nombreArchivoIni = GetApplication().Dynamic af_name_app() + ".ini"
end subroutine

public function boolean of_activa_opcion (datawindow adw_control, string as_opcion);/*
Nombre: of_activa_opcion
Parametros:
	adw_control(datawindow)
	as_opcion(string)
Retorno
	Boolean
*/
Any la_object
Object lo_pag
Boolean lb_retorno = True


la_object = adw_control.GetParent()
lo_pag = la_object.Dynamic TypeOf()

IF lo_pag <> window! THEN
	UserObject luo_page
	String ls_permiso, ls_dw
	
	luo_page = adw_control.GetParent()
	ls_dw = adw_control.ClassName()
	
	SELECT acce_ban_acti INTO :ls_permiso  
	FROM saeacce, saeopci, saeopdw
	WHERE opdw_cod_opci = opci_cod_opci
	AND opdw_cod_opci = acce_cod_opci
	AND opdw_ctr_dawi = acce_ctr_dawi
	AND opci_cod_modu = :ii_modulo
	AND opci_ctr_opci = :as_opcion
	AND opdw_ctr_dawi = :ls_dw
	AND acce_cod_empr = :il_empresa
	AND acce_cod_usua = :il_usuario ;

	IF Trim(ls_permiso) = '0' OR IsNull(ls_permiso) OR ls_permiso = '' THEN 
		luo_page.Enabled = False
		lb_retorno = False
	END IF
END IF

Return lb_retorno

end function

public function boolean of_const_condicion (datawindow adw_actual, ref string as_condiciones, boolean ab_buscar);//	Funcion para contruir las condiciones
//	ingresadas en el DataWindow
Long li_contador, li_total_columnas, ll_taborder
Boolean lb_condicion = False 
String ls_type_col, ls_signo, ls_valor, ls_condicion
Date lda_fecha

IF ab_buscar THEN
adw_actual.SetReDraw(False)
as_condiciones = ""
adw_actual.AcceptText()

// Numero de columnas del DW
li_total_columnas	= Integer(adw_actual.Object.DataWindow.Column.Count)

// Recorre DW para ver condiciones
FOR li_contador = 1 TO li_total_columnas
	ll_taborder = Integer(adw_actual.Describe("#"+String(li_contador) + ".TabSequence"))
	ls_type_col = adw_actual.Describe("#"+String(li_contador) + ".ColType")

	// Lee el valor ingresado
	CHOOSE CASE Left(ls_type_col, 4)
		CASE 'char'
			ls_valor = adw_actual.GetItemString(adw_actual.GetRow(),li_contador)
		CASE 'date'
			IF ls_type_col <> "datetime" THEN
				ls_valor = String(adw_actual.GetItemDate(adw_actual.GetRow(),li_contador))
			ELSE
				ls_valor = String(Date(adw_actual.GetItemDatetime(adw_actual.GetRow(),li_contador)))
			END IF
		CASE 'deci'
			ls_valor = String(adw_actual.GetItemDecimal(adw_actual.GetRow(),li_contador))
		CASE 'long'
			ls_valor = String(adw_actual.GetItemNumber(adw_actual.GetRow(),li_contador))
		CASE 'time'
			ls_valor = String(adw_actual.GetItemTime(adw_actual.GetRow(),li_contador))
	END CHOOSE

	IF NOT IsNull(ls_valor) AND ls_valor <> "" AND ll_taborder >= 0 THEN
		// Construye condicion
		ls_condicion = adw_actual.Describe("#"+String(li_contador) + ".DbName")
		
		CHOOSE CASE Left(ls_type_col, 4)
			CASE 'char'
				ls_signo = " LIKE "
				ls_valor = "'%" + Trim(ls_valor) + "%'"
			CASE 'date'
				ls_signo = " = "
				IF ls_type_col = 'datetime' THEN			// 25-06-2014
					lda_fecha = Date(Left(ls_valor,10))
				ELSE
					lda_fecha = Date(ls_valor)
				END IF
				ls_valor = String(Month(lda_fecha),'00') + '/' + &
					String(Day(lda_fecha),'00') + '/' + &
					String(Year(lda_fecha),'0000')
				ls_condicion = "DATE(" + ls_condicion + ")"
				ls_valor = "'" + Left(ls_valor,10) + "'"
			CASE 'time'
				ls_signo = " = "
				ls_valor = "DATETIME(" + Mid(ls_valor,12) + ") HOUR TO SECOND"	// informix
			CASE ELSE
				ls_signo = " = "
		END CHOOSE
		ls_condicion += ls_signo + ls_valor

		IF lb_condicion THEN
			as_condiciones += ' AND ' + ls_condicion
		ELSE
			as_condiciones += ls_condicion
			lb_condicion = True
		END IF
	END IF
NEXT
adw_actual.SetReDraw(True)
//MESSAGEBOX('sql',as_condiciones)
END IF
RETURN lb_condicion
end function

public subroutine of_cabecera_sucursal (datawindow adw_fuente, integer ai_cabecera);nvo_titulosReporte ltitulosReporte


ltitulosReporte.of_tituloReportes(ai_cabecera, adw_fuente, il_empresa, il_sucursal, gi_finca)

end subroutine

public subroutine of_ayuda ();String ls_manual, ls_path
String ls_aplicacion, ls_archivo, ls_default = ''



ls_aplicacion = GetApplication().Dynamic af_name_app()
ls_archivo = gs_path + ls_aplicacion + ".ini"
ls_path = ProfileString (ls_archivo, ls_aplicacion, "ayuda", ls_default)

IF ls_path = '' THEN
	MessageBox('Error', 'Ingrese path del Acrobat Reader')
	Return
END IF

ls_manual = ls_path + ' ' + gs_path + 'Manuales\' + gnv_ctrdw.is_modulo + '.pdf'
Run(ls_manual)

end subroutine

public function boolean of_verifica_datos (long al_fila, datawindow adw_fuente);/*
Name: of_verifica_datos
	Verifica que todos los campos requeridos sean ingresados
*/
Long ll_num_columnas, ll_cont, ll_tabsequence
Boolean lb_error = True
String ls_style, ls_required, ls_type_col, ls_valor, ls_nombre_campo
Integer li_pos_punto


adw_fuente.AcceptText()
adw_fuente.SetRow(al_fila)
ll_num_columnas = Long(adw_fuente.Object.DataWindow.Column.Count)

FOR ll_cont = 1 TO ll_num_columnas
	ls_style = adw_fuente.Describe("#"+ String(ll_cont) + ".Edit.Style")
	ll_tabsequence = Long(adw_fuente.Describe("#" + String(ll_cont) + ".TabSequence"))
	IF ll_tabsequence = 0 THEN Continue
	
	CHOOSE CASE ls_style
		CASE 'edit'
			ls_required = adw_fuente.Describe("#"+String(ll_cont) + ".Edit.Required")
		CASE 'dddw'
			ls_required = adw_fuente.Describe("#"+String(ll_cont) + ".dddw.Required")
		CASE 'ddlb'
			ls_required = adw_fuente.Describe("#"+String(ll_cont) + ".ddlb.Required")
		CASE 'editmask'
			ls_required = adw_fuente.Describe("#"+String(ll_cont) + ".editmask.Required")
	END CHOOSE

	IF ls_required = "yes" THEN
		adw_fuente.SetColumn(ll_cont)

		// Tipo de dato de la columna
		ls_type_col = adw_fuente.Describe("#"+String(ll_cont) + ".ColType")
		
		// Lee el valor ingresado
		CHOOSE CASE Left(ls_type_col, 4)
			CASE 'char'
				ls_valor = adw_fuente.GetItemString(adw_fuente.GetRow(),ll_cont)
			CASE 'date'
				IF ls_type_col <> "datetime" THEN
					ls_valor = String(adw_fuente.GetItemDate(adw_fuente.GetRow(),ll_cont))
				ELSE
					ls_valor = String(adw_fuente.GetItemDatetime(adw_fuente.GetRow(),ll_cont))
				END IF
			CASE 'deci'
				ls_valor = String(adw_fuente.GetItemDecimal(adw_fuente.GetRow(),ll_cont))
			CASE 'long'
				ls_valor = String(adw_fuente.GetItemNumber(adw_fuente.GetRow(),ll_cont))
			CASE 'time'
				ls_valor = String(adw_fuente.GetItemTime(adw_fuente.GetRow(),ll_cont))
		END CHOOSE
		
		IF IsNull(ls_valor) OR ls_valor = "" THEN
			lb_error = False
			EXIT
		END IF
	END IF
NEXT

IF NOT lb_error THEN
	ls_nombre_campo = adw_fuente.Describe("#"+String(ll_cont) + ".DbName")
	li_pos_punto = Pos(ls_nombre_campo, '.')
	ls_nombre_campo = Mid(ls_nombre_campo, li_pos_punto + 1)

	ls_nombre_campo = adw_fuente.Describe(ls_nombre_campo + "_t.Text")
	f_microhelp('Ingrese ' + ls_nombre_campo)
	Messagebox("Error","Ingrese " + ls_nombre_campo)
	adw_fuente.SetFocus()
	adw_fuente.SetColumn(ll_cont)
END IF	

Return lb_error

end function

public function boolean of_const_condicion (datawindow adw_actual, ref string as_condiciones, boolean ab_buscar, boolean ab_estadoactualizar);//	Funcion para contruir las condiciones
//	ingresadas en el DataWindow
Long li_contador, li_total_columnas, ll_taborder
Boolean lb_condicion = False 
String ls_type_col, ls_signo, ls_valor, ls_condicion, ls_estado_actualizar
Date lda_fecha

IF ab_buscar THEN
adw_actual.SetReDraw(False)
as_condiciones = ""
adw_actual.AcceptText()

// Numero de columnas del DW
li_total_columnas	= Integer(adw_actual.Object.DataWindow.Column.Count)

// Recorre DW para ver condiciones
FOR li_contador = 1 TO li_total_columnas
	ll_taborder = Integer(adw_actual.Describe("#"+String(li_contador) + ".TabSequence"))
	ls_type_col = adw_actual.Describe("#"+String(li_contador) + ".ColType")
	ls_estado_actualizar = adw_actual.Describe("#"+String(li_contador) + ".Criteria.Required")
	IF ab_estadoactualizar AND Lower(ls_estado_actualizar) = 'yes' THEN Continue

	// Lee el valor ingresado
	CHOOSE CASE Left(ls_type_col, 4)
		CASE 'char'
			ls_valor = adw_actual.GetItemString(adw_actual.GetRow(),li_contador)
		CASE 'date'
			IF ls_type_col <> "datetime" THEN
				ls_valor = String(adw_actual.GetItemDate(adw_actual.GetRow(),li_contador))
			ELSE
				ls_valor = String(adw_actual.GetItemDatetime(adw_actual.GetRow(),li_contador))
			END IF
		CASE 'deci'
			ls_valor = String(adw_actual.GetItemDecimal(adw_actual.GetRow(),li_contador))
		CASE 'long'
			ls_valor = String(adw_actual.GetItemNumber(adw_actual.GetRow(),li_contador))
		CASE 'time'
			ls_valor = String(adw_actual.GetItemTime(adw_actual.GetRow(),li_contador))
	END CHOOSE

	IF NOT IsNull(ls_valor) AND ls_valor <> "" AND ll_taborder >= 0 THEN
		// Construye condicion
		ls_condicion = adw_actual.Describe("#"+String(li_contador) + ".DbName")
		
		CHOOSE CASE Left(ls_type_col, 4)
			CASE 'char'
				ls_signo = " LIKE "
				ls_valor = "'%" + Trim(ls_valor) + "%'"
			CASE 'date'
				ls_signo = " = "
				lda_fecha = Date(ls_valor)
				ls_valor = String(Month(lda_fecha),'00') + String(Day(lda_fecha),'00') + String(Year(lda_fecha),'0000')
				ls_condicion = "DATE(" + ls_condicion + ")"
				ls_valor = "'" + Left(ls_valor,10) + "'"
			CASE 'time'
				ls_signo = " = "
				ls_valor = "DATETIME(" + Mid(ls_valor,12) + ") HOUR TO SECOND"	// informix
			CASE ELSE
				ls_signo = " = "
		END CHOOSE
		ls_condicion += ls_signo + ls_valor

		IF lb_condicion THEN
			as_condiciones += ' AND ' + ls_condicion
		ELSE
			as_condiciones += ls_condicion
			lb_condicion = True
		END IF
	END IF
NEXT
adw_actual.SetReDraw(True)
//MESSAGEBOX('sql',as_condiciones)
END IF
RETURN lb_condicion
end function

public subroutine of_cabecera_sucursal (datastore ads_fuente, integer ai_cabecera);nvo_titulosReporte ltitulosReporte


ltitulosReporte.of_tituloReportes(ai_cabecera, ads_fuente, il_empresa, il_sucursal, gi_finca)

end subroutine

public function long of_showerrorbdd (transaction at_transaccionbdd, string as_mensaje);/*
Name: of_showErrorBDD(SQLCA, "No se pudo guardar los cambios.")

Return:
	 1: Exito
	-1: Cuando hay error
*/
String ls_msgError
Long ll_codigoDBError


IF at_transaccionbdd.SqlCode = -1 THEN
	ls_msgError = at_transaccionbdd.SqlErrText
	ll_codigoDBError = at_transaccionbdd.SqlDBCode
	
	RollBack;
	
	MessageBox('Error:' + String(ll_codigoDBError), &
		as_mensaje + "~r~n" + ls_msgError)
	Return -1
END IF
Return 1
end function

public function string of_getnumerofactura (long al_codigoempresa, string as_numeropacking);/*
Name: of_getNumeroFactura
	Retorna el numero de factura comercial segun sea el caso
	
PRMS:
	al_codigoEmpresa
	as_numeroPacking
*/
String ls_numeroFactura


SELECT fpak_num_fact INTO :ls_numeroFactura
FROM saefpak
WHERE fpak_cod_empr = :al_codigoEmpresa
AND fpak_num_pack = :as_numeroPacking;

IF SQLCA.SqlCode = 100 THEN SetNull(ls_numeroFactura)

IF IsNull(ls_numeroFactura) THEN ls_numeroFactura = as_numeroPacking
//IF IsNumber(ls_numeroFactura) THEN ls_numeroFactura = String(Long(ls_numeroFactura))


Return ls_numeroFactura

end function

public subroutine of_formatodocumentoselectronicos ();/*
Name: of_formatoDocumentosElectronicos
	Constriye la cadena para validar el ingreso de los documentos electronicos
	
Create: 24-11-2014

Ejemplo
IF Match('001-001-123456789', gnv_ctrdw.is_formatovalido) THEN
	// Exito
ELSE
	gnv_ctrdw.of_msgError('formatoDE')
	Return 1
END IF
*/
String ls_formato
Long ll_pos, ll_posIni = 1


ls_formato = This.of_getSofiaIni('Documentos Electronicos', 'Formato', '3-3-9')

ll_pos = Pos(ls_formato, '-', ll_posIni)
il_nroDigitoEsta = Long(Left(ls_formato, ll_pos - 1))

ll_posIni = ll_pos + 1
ll_pos = Pos(ls_formato, '-', ll_posIni)
il_nroDigitoPEmi = Long(Mid(ls_formato, ll_posIni, ll_pos - ll_posIni))

il_nroDigitoSecu = Long(Right(ls_formato, Len(ls_formato) - ll_pos))

is_formatoValido = "^" + Fill(cte_soloNumeros, il_nroDigitoEsta * 5) + &
	'-' + Fill(cte_soloNumeros, il_nroDigitoPEmi * 5) + &
	'-' + Fill(cte_soloNumeros, il_nroDigitoSecu * 5) + "$"

is_formatoMsg = "[" + Fill('#', il_nroDigitoEsta) + &
	'-' + Fill('#', il_nroDigitoPEmi) + &
	'-' + Fill('#', il_nroDigitoSecu) + "  " + ls_formato + "]"

end subroutine

public subroutine of_msgerror (string as_error);/*
Name: of_msgError
	Presenta la venta de error segun prm
	
Create: 24-11-2014
*/
String ls_saltoLinea = '~r~n'


CHOOSE CASE as_error
	CASE 'formatoDE'
		MessageBox('Error', 'Formato no válido.' + ls_saltoLinea + 'Debe ingresar: ' + is_formatoMsg, StopSign!)
END CHOOSE
end subroutine

public function string of_getsofiaini (string as_grupo, string as_clave, string as_default);/*
Name: of_getSofiaIni

Create: 24-11-2014

PRMs
	as_grupo
	as_clave
	as_default
*/
String ls_aplicacion, ls_archivo
String ls_valor


ls_aplicacion = GetApplication().Dynamic af_name_app()
as_grupo = 'Documentos Electronicos'
ls_archivo = gs_path + ls_aplicacion + ".ini"

ls_valor = ProfileString (ls_archivo, as_grupo, as_clave, as_default)

Return ls_valor

end function

public function integer of_validanumeroautorizacion (string as_numeroautorizacion);/*
Name: of_validaNumeroAutorizacion

Create: 18-12-2015
*/

IF NOT Match(as_numeroAutorizacion, "^[0-9]+$") THEN
	MessageBox('Error', 'Ingrese solo números.', StopSign!)
	Return -1
END IF

IF NOT (Len(as_numeroAutorizacion) = 10 OR &
	Len(as_numeroAutorizacion) = 37 OR &
	Len(as_numeroAutorizacion) = 49) THEN
	MessageBox('Error', 'Número de autorización no valido.~r~nLongitud debe ser 10, 37 o 49.', StopSign!)
	Return -1
END IF


Return 1
end function

public function string of_validanombre (string as_nombregeneral, integer ai_numerocaracteres);/*
Name: of_validaNombre
	Valida el nombre para poder guabar en windows
Create: 15-01-2016

PRMS
	as_nombreGeneral: Nombre a verificar
	ai_numeroCaracteres: Numero de caracteres que se escoge del nombre
*/
String ls_auxiliar
String ls_signo[] = {'\','/',':','*','?','<','>','|','"'}
String ls_caracterNew = ''
Long ll_pos, ll_cont, ll_totalSignos


ls_auxiliar = Mid(as_nombreGeneral, 1, 20)

ll_totalSignos = UpperBound(ls_signo)
FOR ll_cont = 1 TO ll_totalSignos
	// Encuentra la primera ocurrencia de ls_signo.
	ll_pos = 1
	ll_pos = Pos(ls_auxiliar, ls_signo[ll_cont], ll_pos)
	
	// Solo ingresa al bucle si encuentra ls_signo.
	DO WHILE ll_pos > 0
		// Reemplaza ls_signo con ls_caracterNew.
		ls_auxiliar = Replace(ls_auxiliar, ll_pos, Len(ls_signo[ll_cont]), ls_caracterNew)
		
		// Encuentra la siguiente ocurrencia de ls_signo.
		ll_pos = Pos(ls_auxiliar, ls_signo[ll_cont], ll_pos + Len(ls_caracterNew))
	LOOP
END FOR

Return ls_auxiliar

end function

public function integer of_validarclaveacceso (string as_numeroautorizacion);/*
Name: of_validarClaveAcceso

Create: 22-08-2016
*/


IF NOT Match(as_numeroAutorizacion, "^[0-9]+$") THEN
	MessageBox('Error', 'Ingrese solo números.', StopSign!)
	Return -1
END IF

IF Len(as_numeroAutorizacion) <> 49 THEN
	MessageBox('Error', 'Clave de acceso no valida.~r~nLongitud debe ser 49.', StopSign!)
	Return -1
END IF


Return 1
end function

public function string of_gettitulomodulo ();/*
Name: of_getTituloModulo

Create: 28-04-2017
*/
String ls_titulo = "Sofia  ", ls_sucursal, ls_ciudad, ls_empresa, ls_finca
String ls_nombreBodega
Integer li_codigoGrupo

Constant String ESPACIO_01 = Space(05)
Constant String ESPACIO_02 = Space(05)


SELECT sucu_nom_sucu, ciud_nom_ciud ,empr_nom_empr
INTO :ls_sucursal, :ls_ciudad, :ls_empresa 
FROM saeciud, saesucu ,saeempr 
WHERE ciud_cod_ciud = sucu_cod_ciud
and empr_cod_empr=sucu_cod_empr
AND sucu_cod_empr = :il_empresa
AND sucu_cod_sucu = :il_sucursal;

// Verifica grupo
SELECT modu_cod_paqu INTO :li_codigoGrupo
FROM saemodu
WHERE modu_cod_modu = :ii_modulo;

//ls_titulo += Trim(is_modulo) + ESPACIO_01 + "- Empresa - " + Trim(ls_empresa)
ls_titulo += Trim(is_modulo) + ESPACIO_01 + "- Empresa: " + Trim(ls_empresa)

CHOOSE CASE li_codigoGrupo
	CASE 3,5
		SELECT finc_nom_finc INTO :ls_finca
		FROM saefinc
		WHERE finc_cod_finc = :gi_finca
		AND finc_cod_empr = :gnv_ctrdw.il_empresa;
		
		IF NOT IsNull(ls_finca) AND ls_finca <> '' THEN
//			ls_titulo += ESPACIO_02 + "Finca - ( " + Trim(ls_finca) + " )"
			ls_titulo += ESPACIO_02 + "Finca: ( " + Trim(ls_finca) + " )"
		END IF
	CASE ELSE
		/*
		ls_titulo += ESPACIO_02+"- Sucursal - " + Trim(ls_sucursal) + &
						 ESPACIO_02 + "( " + ls_ciudad + " )"
		*/
		IF ii_modulo = 10 OR ii_modulo = 7 THEN	// 23-05-2018
			SELECT bode_nom_bode INTO :ls_nombreBodega
			FROM saebode
			WHERE bode_cod_bode = :il_bodega
			AND bode_cod_empr = :il_empresa;
			
			IF IsNull(ls_nombreBodega) OR ls_nombreBodega = '' THEN ls_nombreBodega = 'NO EXISTE'
			ls_titulo += ESPACIO_02 + "Bodega - ( " + ls_nombreBodega + " )"
		ELSE
			ls_titulo += ESPACIO_02+"- Sucursal - " + Trim(ls_sucursal) + &
							 ESPACIO_02 + "( " + ls_ciudad + " )"
		END IF
END CHOOSE

ls_titulo = WordCap(ls_titulo)
//messagebox(string(len(ls_titulo)), ls_titulo)
Return ls_titulo

end function

public function integer msg_err (string as_msg);/*
Nombre: msg_err
	Despliega una pantalla de mensaje de error
Prms:
	as_msg: Mensaje
Return
	-1: Salir del proceso en que fue llamado
*/

MessageBox('Error',as_msg, StopSign!)
Return -1

end function

public subroutine of_inserta_blanco_dddw (datawindow adw_control);/*
Nombre: of_inserta_blanco_dddw

Create: 04-09-2002
Modify: 04-09-2002
*/
DataWindowChild ldwc_dddw
String ls_data, ls_nombre_columna
Integer li_columnas

ls_data = adw_control.Object.DataWindow.Column.Count

FOR li_columnas = 1 TO Integer(ls_data)
	ls_nombre_columna = adw_control.Describe("#" + string(li_columnas) + ".Name")
	IF adw_control.GetChild(ls_nombre_columna, ldwc_dddw) = 1 THEN
		ldwc_dddw.SetTransObject(SQLCA)
		ldwc_dddw.InsertRow(0)
	END IF
NEXT


end subroutine

public function string of_getempresaini (string as_grupo, string as_clave, string as_default);/*
Name: of_getEmpresaIni

Create: 16-08-2018

PRMs
	as_grupo			Documentos Electronicos
	as_clave
	as_default
*/
String ls_valor
nvo_valores_iniciales lvalores_iniciales

ls_valor = ProfileString (lvalores_iniciales.of_getNombreArchivo(il_empresa), &
							as_grupo, &
							as_clave, &
							as_default)

Return ls_valor

end function

public subroutine of_poswinfinal (window aw_actual);/*
Name: of_posWinFinal

Autor : Juan Pablo
Create: 14-05-2020

PRMS:
	aw_actual
*/
nvo_configuraciones lconfig
String ls_aplicacion
String ls_pathRegistro
String ls_verWindows


ls_aplicacion = GetApplication().Dynamic af_name_app()

// Archivo ini
ls_verWindows = lconfig.of_getVerWin()


// Resgistro
CHOOSE CASE ls_verWindows
	CASE "7"
		ls_pathRegistro = "HKEY_LOCAL_MACHINE\Software\Natuflor\" + ls_aplicacion + "\Windows\" + aw_actual.ClassName()
	CASE "10"
		ls_pathRegistro = "HKEY_CURRENT_USER\Software\Classes\VirtualStore\MACHINE\SOFTWARE\WOW6432Node\Natuflor\" + &
		                  ls_aplicacion + "\Windows\" + aw_actual.ClassName()
END CHOOSE

RegistrySet(ls_pathRegistro, "Ancho", ReguLong!, aw_actual.width)
RegistrySet(ls_pathRegistro, "Alto" , ReguLong!, aw_actual.height)
RegistrySet(ls_pathRegistro, "X"    , ReguLong!, aw_actual.x)
RegistrySet(ls_pathRegistro, "Y"    , ReguLong!, aw_actual.y)

end subroutine

public subroutine of_poswininicio (window aw_actual);/*
Name: of_posWinInicio

Autor : Juan Pablo
Create: 14-05-2020

PRMS:
	aw_actual
*/
nvo_configuraciones lconfig
String ls_aplicacion
String ls_pathRegistro
String ls_verWindows
ulong  lu_ancho, lu_alto, lu_x, lu_y


ls_aplicacion = GetApplication().Dynamic af_name_app()

// Archivo ini
ls_verWindows = lconfig.of_getVerWin()


// Resgistro
CHOOSE CASE ls_verWindows
	CASE "7"
		ls_pathRegistro = "HKEY_LOCAL_MACHINE\Software\Natuflor\" + ls_aplicacion + "\Windows\" + aw_actual.ClassName()
	CASE "10"
		ls_pathRegistro = "HKEY_CURRENT_USER\Software\Classes\VirtualStore\MACHINE\SOFTWARE\WOW6432Node\Natuflor\" + &
		                  ls_aplicacion + "\Windows\" + aw_actual.ClassName()
END CHOOSE

RegistryGet(ls_pathRegistro, "Ancho", ReguLong!, lu_ancho)
RegistryGet(ls_pathRegistro, "Alto" , ReguLong!, lu_alto)
RegistryGet(ls_pathRegistro, "X"    , ReguLong!, lu_x)
RegistryGet(ls_pathRegistro, "Y"    , ReguLong!, lu_y)

IF lu_alto > 0 OR lu_ancho > 0 THEN
	aw_actual.Move (lu_x, lu_y)
	aw_actual.Resize (lu_ancho, lu_alto)
END IF

end subroutine

public function integer of_exportarconlogo (datawindow adw_datos, string as_nombrearchivo, string as_carpetadestino);/*
Name: of_exportarConLogo
	
	
Autor : Juan Pablo
Create: 39-04-2021

PRMS:
	adw_datos
	as_nombreArchivo
	as_carpetaDestino
*/
String  ls_pathTMP
String  ls_nombreImpresora
String  ls_archivoOrigen, ls_archivoDestino
String  ls_nomArchivoAux
Boolean lb_existe
Integer li_ret


// Verifica impresora
ls_nombreImpresora = ProfileString(gs_path + is_nombreArchivoINI,'Documentos Electronicos','impresora','')
IF ls_nombreImpresora = '' THEN
	MessageBox('Error', 'Impresora PDF no definida.')
	Return -1
END IF


// Verifica que la carpeta tmp este creada
ls_pathTMP = gs_path + vg.A-TMP-PDF
IF NOT DirectoryExists ( ls_pathTMP ) THEN
	IF CreateDirectory ( ls_pathTMP ) = -1 THEN Return -1
END IF


// Default
ls_nomArchivoAux  = Trim(Mid(as_nombreArchivo,1,31))
ls_archivoOrigen  = ls_pathTMP + ls_nomArchivoAux + '.pdf'
ls_archivoDestino = as_carpetaDestino + as_nombreArchivo + '.pdf'


// Envia a la impresora PDFCreator y espera que se cree el archivo
adw_datos.Modify("DataWindow.Print.DocumentName='" + ls_nomArchivoAux   + "'")
adw_datos.Modify("DataWindow.Print.PrinterName='"  + ls_nombreImpresora + "'")

adw_datos.Print()


lb_existe = FileExists (ls_archivoOrigen)
DO WHILE NOT lb_existe
	lb_existe = FileExists (ls_archivoOrigen)
LOOP

Sleep(1)


// Copia y elimina el archivo temporal
li_ret = FileCopy(ls_archivoOrigen, ls_archivoDestino, True)
IF li_ret < 0 THEN
	MessageBox('Error ' + String(li_ret), 'Archivo no pudo ser creado.')
	Return -1
END IF

IF FileExists (ls_archivoOrigen) THEN
	lb_existe = FileDelete(ls_archivoOrigen)
END IF


Return 1

//adw_datos.Modify("p_logo.Filename='" + is_nombreArchivoLog + "'")	// 15-08-2018

end function

public function integer of_exportarconlogo (datastore ads_datos, string as_nombrearchivo, string as_carpetadestino);/*
Name: of_exportarConLogo
	
	
Autor : Juan Pablo
Create: 39-04-2021

PRMS:
	ads_datos
	as_nombreArchivo
	as_carpetaDestino
*/
String  ls_pathTMP
String  ls_nombreImpresora
String  ls_archivoOrigen, ls_archivoDestino
String  ls_nomArchivoAux
Boolean lb_existe
Integer li_ret


// Verifica que la carpeta tmp este creada
ls_pathTMP = gs_path + vg.A-TMP-PDF
IF NOT DirectoryExists ( ls_pathTMP ) THEN
	IF CreateDirectory ( ls_pathTMP ) = -1 THEN Return -1
END IF


// Default
ls_nomArchivoAux  = Trim(Mid(as_nombreArchivo,1,31))
ls_archivoOrigen  = ls_pathTMP + ls_nomArchivoAux + '.pdf'
ls_archivoDestino = as_carpetaDestino + as_nombreArchivo + '.pdf'


// Envia a la impresora PDFCreator y espera que se cree el archivo
ls_nombreImpresora = ProfileString(is_nombreArchivoINI,'Documentos Electronicos','impresora','')

ads_datos.Modify("DataWindow.Print.DocumentName='" + ls_nomArchivoAux   + "'")
ads_datos.Modify("DataWindow.Print.PrinterName='"  + ls_nombreImpresora + "'")

ads_datos.Print()


lb_existe = FileExists (ls_archivoOrigen)
DO WHILE NOT lb_existe
	lb_existe = FileExists (ls_archivoOrigen)
LOOP

Sleep(1)


// Copia y elimina el archivo temporal
li_ret = FileCopy(ls_archivoOrigen, ls_archivoDestino, True)
IF li_ret < 0 THEN
	MessageBox('Error ' + String(li_ret), 'Archivo no pudo ser creado.')
	Return -1
END IF

IF FileExists (ls_archivoOrigen) THEN
	lb_existe = FileDelete(ls_archivoOrigen)
END IF


Return 1

//ads_datos.Modify("p_logo.Filename='" + is_nombreArchivoLog + "'")	// 15-08-2018

end function

on nvo_controles_datawin.create
call super::create
TriggerEvent( this, "constructor" )
end on

on nvo_controles_datawin.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

